{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
module Control.Plan (module Control.Plan) where

import Prelude hiding ((.),id)
import Data.Tree
import Data.Profunctor (Profunctor(..),Star(..))
import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Arrow
import Streaming.Prelude (Stream,Of(..),yield)

data Plan w s m a b = Plan (Steps w s) (Star (Stream (Of (Tick,s)) m) a b) deriving Functor

instance (Monoid w,Monad m) => Applicative (Plan w s m a) where
    pure x = Plan mempty (pure x)
    Plan forest1 f <*> Plan forest2 x = Plan (forest1 `mappend` forest2) (f <*> x)

instance (Monoid w,Monad m) => Category (Plan w s m) where
    id = Plan mempty (Star (runKleisli id))
    (Plan forest1 (Star f1)) . (Plan forest2 (Star f2)) = 
        Plan (forest2 `mappend` forest1) (Star (f2 >=> f1))

instance (Monoid w,Monad m) => Arrow (Plan w s m) where
    arr f = Plan mempty (Star (runKleisli (arr f)))
    first (Plan forest (Star f)) =  Plan forest (Star (runKleisli (first (Kleisli f))))

instance (Monoid w,Monad m) => Profunctor (Plan w s m) where
    lmap f p = f ^>> p
    rmap f p = p >>^ f

data Steps w e = Steps w [(e, Steps w e,w)] deriving Functor

instance Monoid w => Monoid (Steps w e) where
    mempty = Steps mempty [] 
    Steps w1 [] `mappend` Steps w2 s2 = Steps (w1 `mappend` w2) s2
    Steps w1 s1 `mappend` Steps w2 s2 = Steps w1 (mappendAtTheEnd s1 w2 ++ s2)
        where
        mappendAtTheEnd ((e,s,w'):[]) w = (e,s,w' `mappend` w) : []
        mappendAtTheEnd (l:ls)        w = l : mappendAtTheEnd ls w
        mappendAtTheEnd []            _ = error "should never happen"

data Tick = Starting | Finished deriving (Eq,Ord,Enum,Show)

getSteps :: Plan w s m a b -> Steps w s
getSteps (Plan forest _) = forest

runPlan :: Plan w s m a b -> a -> Stream (Of (Tick,s)) m b
runPlan (Plan _ (Star f)) = f

step :: (Monoid w,Monad m) => s -> Plan w s m a b -> Plan w s m a b
step s (Plan forest (Star f)) = 
    Plan (Steps mempty [(s,forest,mempty)]) 
         (Star (\x -> yield (Starting,s) *> f x <* yield (Finished,s)))

foretell :: (Monoid w,Monad m) => w -> Plan w s m a ()
foretell w = Plan (Steps w []) (pure ())  

plan :: (Monoid w,Monad m) => m b -> Plan w s m a b
plan x = Plan mempty (Star (const (lift x))) 

planIO :: (Monoid w,MonadIO m) => IO b -> Plan w s m a b
planIO x = Plan mempty (Star (const (liftIO x))) 

planK :: (Monoid w,Monad m) => (a -> m b) -> Plan w s m a b
planK f = Plan mempty (Star (lift . f)) 

planKIO :: (Monoid w,MonadIO m) => (a -> IO b) -> Plan w s m a b
planKIO f = Plan mempty (Star (liftIO . f)) 

