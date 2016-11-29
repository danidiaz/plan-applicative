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

data Progress s = Starting s | Finished s deriving (Eq,Ord,Show)

data Plan s m a b = Plan (Forest s) (Star (Stream (Of (Progress s)) m) a b) deriving Functor

getSteps :: Plan s m a b -> Forest s
getSteps (Plan forest _) = forest

runPlan :: Plan s m a b -> a -> Stream (Of (Progress s)) m b
runPlan (Plan _ (Star f)) = f

instance Monad m => Applicative (Plan s m a) where
    pure x = Plan [] (pure x)
    Plan forest1 f <*> Plan forest2 x = Plan (forest1 ++ forest2) (f <*> x)

instance Monad m => Category (Plan s m) where
    id = Plan [] (Star (runKleisli id))
    (Plan forest1 (Star f1)) . (Plan forest2 (Star f2)) = 
        Plan (forest2 ++ forest1) (Star (f2 >=> f1))

instance Monad m => Arrow (Plan s m) where
    arr f = Plan [] (Star (runKleisli (arr f)))
    first (Plan forest (Star f)) =  Plan forest (Star (runKleisli (first (Kleisli f))))

instance Monad m => Profunctor (Plan s m) where
    lmap f plan = f ^>> plan
    rmap f plan = plan >>^ f

step :: Monad m => s -> Plan s m a b -> Plan s m a b
step s (Plan forest (Star f)) = 
    Plan [Node s forest] 
         (Star (\x -> yield (Starting s) *> f x <* yield (Finished s)))

plan :: Monad m => m b -> Plan s m a b
plan x = Plan [] (Star (const (lift x))) 

planIO :: MonadIO m => IO b -> Plan s m a b
planIO x = Plan [] (Star (const (liftIO x))) 

planK :: Monad m => (a -> m b) -> Plan s m a b
planK f = Plan [] (Star (lift . f)) 

planKIO :: MonadIO m => (a -> IO b) -> Plan s m a b
planKIO f = Plan [] (Star (liftIO . f)) 

    -- first (Plan forest star)  *** (Plan forest2 star2) = Plan (forest1 ++ forest2) (liftA2 (,) star1 star2)

-- foretell :: Monoid w => Plan p w m a b -> p -> Plan p w m a b
-- foretell (Plan ts k) r = Plan (Forest' mempty [Tree' r ts] mempty) k
-- 
-- preface :: Monoid w => Plan p w m a b -> w -> Plan p w m a b
-- preface (Plan (Forest' w1 p w2) k) w = Plan (Forest' (w <> w1) p w2) k
-- 
-- coda :: Monoid w => Plan p w m a b -> w -> Plan p w m a b
-- coda (Plan (Forest' w1 p w2) k) w = Plan (Forest' w1 p (w2 <> w)) k

-- tracker-arrow
-- Control.Arrow.Tracker
-- step
-- before
-- after
