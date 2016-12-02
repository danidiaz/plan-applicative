{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
module Control.Plan.Core (module Control.Plan.Core) where

import Prelude hiding ((.),id)
import Data.Tree
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
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

data Steps w e = Steps w (Seq (e, Steps w e,w)) deriving Functor

instance Monoid w => Monoid (Steps w e) where
    mempty = Steps mempty mempty
    Steps w1 s1 `mappend` Steps w2 s2 = 
        case Seq.viewr s1 of
            Seq.EmptyR          -> Steps (w1 `mappend` w2) s2
            s1' Seq.:> (e,s,w') -> Steps w1 ((s1' Seq.|> (e,s,w' `mappend` w2)) `mappend` s2)

data Tick = Starting | Finished deriving (Eq,Ord,Enum,Show)

getSteps :: Plan w s m a b -> Steps w s
getSteps (Plan forest _) = forest

runPlan :: Plan w s m a b -> a -> Stream (Of (Tick,s)) m b
runPlan (Plan _ (Star f)) = f

step :: (Monoid w,Monad m) => s -> Plan w s m a b -> Plan w s m a b
step s (Plan forest (Star f)) = 
    Plan (Steps mempty (Seq.singleton (s,forest,mempty))) 
         (Star (\x -> yield (Starting,s) *> f x <* yield (Finished,s)))

foretell :: (Monad m) => w -> Plan w s m a ()
foretell w = Plan (Steps w mempty) (pure ())  

plan :: (Monoid w,Monad m) => m b -> Plan w s m a b
plan x = Plan mempty (Star (const (lift x))) 

planIO :: (Monoid w,MonadIO m) => IO b -> Plan w s m a b
planIO x = Plan mempty (Star (const (liftIO x))) 

planK :: (Monoid w,Monad m) => (a -> m b) -> Plan w s m a b
planK f = Plan mempty (Star (lift . f)) 

planKIO :: (Monoid w,MonadIO m) => (a -> IO b) -> Plan w s m a b
planKIO f = Plan mempty (Star (liftIO . f)) 

-- TODO:
-- foldSteps catamorphism
-- stepsToTree
-- some kind of run-in-io function to avoid having to always import streaming  
-- ArrowChoice instance? 
