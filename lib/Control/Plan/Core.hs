-- | If you  manipulate the internals of `Plan` to add fake steps, bad things
-- might happen.

{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language RankNTypes #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
module Control.Plan.Core (module Control.Plan.Core) where

import Prelude hiding ((.),id)
import qualified Data.Bifunctor as Bifunctor
import Data.Bifunctor(Bifunctor)
import Data.Tree
import Data.Monoid
import Data.List.NonEmpty (NonEmpty((:|)),(<|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Foldable
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Identity
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Profunctor (Profunctor(..),Star(..))
import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Arrow
import Streaming (hoist)
import Streaming.Prelude (Stream,Of(..),yield,next,effects)

data Plan w s m a b = Plan (Steps w s) (Star (Stream (Of Tick_) m) a b) deriving Functor

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

data Steps w e = Steps w (Seq (e, Steps w e,w)) deriving (Functor,Foldable,Traversable)

instance Bifunctor Steps where
    first f (Steps w steps) = 
        let withStep (e,substeps,w') = (e,Bifunctor.first f substeps,f w') 
        in  Steps (f w) (fmap withStep steps)  
    second = fmap

instance Bifoldable Steps where
    bifoldMap f g (Steps w steps) = 
        f w `mappend` foldMap (\(e,substeps,w') -> g e 
                                                   `mappend` 
                                                   bifoldMap f g substeps
                                                   `mappend` 
                                                   f w') steps

instance Bitraversable Steps where
    bitraverse f g (Steps w steps) = 
        Steps <$> f w <*> traverse innertraverse steps  
        where
        innertraverse (e,substeps,w') = (,,) <$> g e <*> bitraverse f g substeps <*> f w' 
    
instance Monoid w => Monoid (Steps w e) where
    mempty = Steps mempty mempty
    Steps w1 s1 `mappend` Steps w2 s2 = 
        case Seq.viewr s1 of
            Seq.EmptyR          -> Steps (w1 `mappend` w2) s2
            s1' Seq.:> (e,s,w') -> Steps w1 ((s1' Seq.|> (e,s,w' `mappend` w2)) `mappend` s2)

foldSteps :: (w -> Seq (e,r,w) -> r) -> Steps w e -> r
foldSteps f = go
    where
    go (Steps w steps) = f w (fmap (\(e',steps',w') -> (e',go steps',w')) steps)

bimapSteps :: (w -> w') -> (e -> e') -> Plan w e m a b -> Plan w' e' m a b
bimapSteps f g (Plan steps star) = Plan (Bifunctor.bimap f g steps) star

zoomSteps :: Monoid w' => ((w -> Identity w) -> w' -> Identity w') -> Plan w e m a b -> Plan w' e m a b
zoomSteps setter = bimapSteps (\w -> set' w mempty) id
    where
    set' w = runIdentity . setter (Identity . const w)

hoistPlan :: Monad m => (forall x. m x -> n x) -> Plan w e m a b -> Plan w e n a b
hoistPlan trans (Plan steps (Star f)) = Plan steps (Star (hoist trans . f)) 

data Tick_ = Skipping_ | Starting_ | Finished_ deriving (Eq,Ord,Enum,Show)

getSteps :: Plan w s m a b -> Steps w s
getSteps (Plan forest _) = forest

stepsToForest :: Steps w s -> Forest s
stepsToForest (Steps _ steps) = map toNode (toList steps) 
    where
    toNode (e,steps',_) = Node e (stepsToForest steps')

runPlan :: Monad m 
        => Plan w s m a b -- ^ 
        -> a 
        -> Stream (Of (Change () () s)) m b
runPlan plan a = snd <$> runPlanWith (return ()) (return ()) plan a

step :: (Monoid w,Monad m) => s -> Plan w s m a b -> Plan w s m a b
step s (Plan forest (Star f)) = 
    Plan (Steps mempty (Seq.singleton (s,forest,mempty))) 
         (Star (\x -> yield Starting_ *> f x <* yield Finished_))

skippable :: (Monoid w,Monad m) => s -> Plan w s m a () -> Plan w s m (Maybe a) ()
skippable s (Plan forest (Star f)) = 
    Plan (Steps mempty (Seq.singleton (s,forest,mempty))) 
         (Star (\m -> case m of
                        Just x -> yield Starting_ *> f x <* yield Finished_
                        Nothing -> yield Skipping_))

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

zipSteps' :: Forest a -> Steps w r -> Maybe (Steps w (a,r))
zipSteps' forest (Steps w substeps) 
    | length forest == length substeps = 
        let paired = Seq.zipWith (\(Node a subforest) (e,substeps',w') -> 
                                        ((a,e),zipSteps' subforest substeps',w'))
                                 (Seq.fromList forest) 
                                 substeps 
        in Steps w <$> traverse (\(e,ms,w') -> fmap (\s -> (e,s,w')) ms) paired 
    | otherwise = Nothing

zipSteps :: Forest a -> Plan w r m i o -> Maybe (Plan w (a,r) m i o)
zipSteps forest (Plan steps star) = Plan <$> zipSteps' forest steps <*> pure star 

data Change start end c = Starting (NonEmpty (Context start end c)) (Forest c) 
                        | Finished (NonEmpty (Context start end c)) (Forest ((start,end),c)) end 
                        deriving (Eq,Show,Functor)

changeToForest :: Change start end c -> Forest (Maybe (start,Maybe end),c)
changeToForest (Starting contexts pending) = 
    foldr contextToForest (pendingToForest pending) contexts 
changeToForest (Finished (Context completed' (start',current') pending' :| contexts) completed end) = 
    foldr contextToForest [Node (Just (start',Just end),current') (completedToForest completed)] contexts 

contextToForest :: Context start end c -> Forest (Maybe (start,Maybe end),c) -> Forest (Maybe (start,Maybe end),c) 
contextToForest (Context completed' (start',current') pending') below =
    completedToForest completed' ++ [Node (Just (start',Nothing),current') below] ++ pendingToForest pending'

completedToForest :: Forest ((start,end),c) -> Forest (Maybe (start,Maybe end),c) 
completedToForest (reverse -> forest) = map (fmap (\((start,end),c) -> (Just (start,Just end),c))) forest

pendingToForest :: Forest c -> Forest (Maybe (start,Maybe end),c) 
pendingToForest forest = map (fmap (\c -> (Nothing,c))) forest

data Context start end c = Context
                         {
                           completed :: Forest ((start,end),c)
                         , current :: (start,c)
                         , pending :: Forest c
                         } deriving (Show,Eq,Functor)

runPlanWith :: Monad m 
            => m start -- ^
            -> m end 
            -> Plan w s m a b 
            -> a 
            -> Stream (Of (Change start end s)) m (Forest ((start,end),s),b)
runPlanWith startMeasure finishMeasure (Plan steps (Star f)) initial = 
      let go state stream = 
            do n <- lift (next stream)
               case (n,state) of 
                   (Left b,RunState completed [] []) -> 
                       return (reverse completed,b) 
                   (Right (Starting_,stream'),
                    RunState completed (Node root subforest:forest) upwards) -> 
                       do startRead <- lift startMeasure
                          let tip = Context completed (startRead,root) forest
                          yield (Starting (tip :| upwards) subforest)
                          go (RunState [] subforest (tip : upwards)) 
                             stream'
                   (Right (Finished_,stream'),
                    RunState completed [] (m@(Context recap (startRead,root) pending):upwards)) -> 
                       do finishRead <- lift finishMeasure
                          let reversed = reverse completed
                          yield (Finished (m :| upwards) reversed finishRead)  
                          go (RunState (Node ((startRead,finishRead),root) reversed : recap) pending upwards) 
                             stream'
                   _ -> error "should never happen"
      in go (RunState [] (stepsToForest steps) []) (f initial)

data RunState start end c = RunState !(Forest ((start,end),c)) !(Forest c) ![Context start end c]

unliftPlan :: Monad m => Plan w s m i o -> i -> m o
unliftPlan plan i = snd <$> effects (runPlanWith (pure ()) (pure ()) plan i)

data Recap measure chapter = Recap 
                           { 
                             before :: Seq (chapter,measure,Either (Forest chapter) (Recap measure chapter)) 
                           , instant :: measure
                           }

data Context' measure c = Context'
                        {
                          completed' :: Recap measure c
                        , current' :: c
                        , pending' :: Forest c
                        } 

data RunState' measure c = RunState' !(Seq (c,measure,Either (Forest c) (Recap measure c)))
                                     !(Forest c) 
                                     ![Context' measure c]

data Progress' measure c = Progress (NonEmpty (Context' measure c)) (StepEvent measure c) 

data StepEvent measure c = Skipped'  (Forest c)
                         | Starting' (Forest c)
                         | Finished' (Recap measure c)


runPlan' :: Monad m 
            => m measure -- ^
            -> Plan w s m a b 
            -> a 
            -> Stream (Of (Progress' measure s)) m (Recap measure s,b)
runPlan' makeMeasure (Plan steps (Star f)) initial = 
      let go state stream = 
            do n <- lift (next stream)
               case (n,state) of 
                   (Left b,RunState' completed [] []) -> do 
                       measure <- lift makeMeasure
                       return (Recap completed measure,b) 
                   (Right (Skipping_,stream'),_) -> 
                        undefined
                   (Right (Starting_,stream'),
                    RunState' completed (Node root subforest:forest) upwards) -> 
                        undefined
--                       do startRead <- lift startMeasure
--                          let tip = Context completed (startRead,root) forest
--                          yield (Starting (tip :| upwards) subforest)
--                          go (RunState [] subforest (tip : upwards)) 
--                             stream'
                   (Right (Finished_,stream'),_) -> 
                        undefined
--                    RunState completed [] (m@(Context recap (startRead,root) pending):upwards)) -> 
--                       do finishRead <- lift finishMeasure
--                          let reversed = reverse completed
--                          yield (Finished (m :| upwards) reversed finishRead)  
--                          go (RunState (Node ((startRead,finishRead),root) reversed : recap) pending upwards) 
--                             stream'
                   _ -> error "should never happen"
      in go (RunState' mempty (stepsToForest steps) []) (f initial)



-- TODO Some kind of run-in-io function to avoid having to always import streaming  
-- TODO Actually implement the logic for Finished'
-- TODO Modify the steps for Finished'.
-- TODO Use only one measurement.
