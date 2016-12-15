-- | If you  manipulate thr internals of `Plan` to add fake steps, bad things
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

data Plan s w m a b = Plan (Steps s w) (Star (Stream (Of Tick_) m) a b) deriving Functor

instance (Monoid w,Monad m) => Applicative (Plan s w m a) where
    pure x = Plan mempty (pure x)
    Plan forest1 f <*> Plan forest2 x = Plan (forest1 `mappend` forest2) (f <*> x)

instance (Monoid w,Monad m) => Category (Plan s w m) where
    id = Plan mempty (Star (runKleisli id))
    (Plan forest1 (Star f1)) . (Plan forest2 (Star f2)) = 
        Plan (forest2 `mappend` forest1) (Star (f2 >=> f1))

instance (Monoid w,Monad m) => Arrow (Plan s w m) where
    arr f = Plan mempty (Star (runKleisli (arr f)))
    first (Plan forest (Star f)) =  Plan forest (Star (runKleisli (first (Kleisli f))))

instance (Monoid w,Monad m) => Profunctor (Plan s w m) where
    lmap f p = f ^>> p
    rmap f p = p >>^ f

data Steps e w = Steps (Seq (w,e,Steps e w)) w deriving (Functor,Foldable,Traversable)

instance Bifunctor Steps where
    first f (Steps steps w) = 
        let withStep (w',e,substeps) = (w',f e,Bifunctor.first f substeps) 
        in  Steps (fmap withStep steps) w
    second = fmap

instance Bifoldable Steps where
    bifoldMap g f (Steps steps w) = 
        foldMap (\(w',e,substeps) -> f w'
                                     `mappend` 
                                     g e 
                                     `mappend` 
                                     bifoldMap g f substeps) steps
        `mappend`
        f w

instance Bitraversable Steps where
    bitraverse g f (Steps steps w) = 
        Steps <$> traverse innertraverse steps <*> f w
        where
        innertraverse (w',e,substeps) = (,,) <$> f w' <*> g e <*> bitraverse g f substeps
    
instance Monoid w => Monoid (Steps e w) where
    mempty = Steps mempty mempty
    Steps s1 w1 `mappend` Steps s2 w2 = 
        case Seq.viewl s2 of
            Seq.EmptyL          -> Steps s1 (w1 `mappend` w2)
            (w',e,s) Seq.:< s2' -> Steps (s1 `mappend` ((w1 `mappend` w',e,s) Seq.<| s2')) w2

foldSteps :: (Seq (w,e,r) -> w -> r) -> Steps e w -> r
foldSteps f = go
    where
    go (Steps steps w) = f (fmap (\(w',e',steps') -> (w',e',go steps')) steps) w

bimapSteps ::  (e -> e') -> (w -> w') -> Plan e w m a b -> Plan e' w' m a b
bimapSteps f g (Plan steps star) = Plan (Bifunctor.bimap f g steps) star

zoomSteps :: Monoid w' => ((w -> Identity w) -> w' -> Identity w') -> Plan e w m a b -> Plan e w' m a b
zoomSteps setter = bimapSteps id (\w -> set' w mempty)
    where
    set' w = runIdentity . setter (Identity . const w)

hoistPlan :: Monad m => (forall x. m x -> n x) -> Plan e w m a b -> Plan e w n a b
hoistPlan trans (Plan steps (Star f)) = Plan steps (Star (hoist trans . f)) 

data Tick_ = Skipping_ | Started_ | Finished_ deriving (Eq,Ord,Enum,Show)

getSteps :: Plan s w m a b -> Steps s w
getSteps (Plan forest _) = forest

stepsToForest :: Steps s w -> Forest s
stepsToForest (Steps steps _) = map toNode (toList steps) 
    where
    toNode (_,e,steps') = Node e (stepsToForest steps')

step :: (Monoid w,Monad m) => s -> Plan s w m a b -> Plan s w m a b
step s (Plan forest (Star f)) = 
    Plan (Steps (Seq.singleton (mempty,s,forest)) mempty) 
         (Star (\x -> yield Started_ *> f x <* yield Finished_))

skippable :: (Monoid w,Monad m) => s -> Plan s w m a () -> Plan s w m (Maybe a) ()
skippable s (Plan forest (Star f)) = 
    Plan (Steps (Seq.singleton (mempty,s,forest)) mempty) 
         (Star (\m -> case m of
                        Just x -> yield Started_ *> f x <* yield Finished_
                        Nothing -> yield Skipping_))

foretell :: (Monad m) => w -> Plan s w m a ()
foretell w = Plan (Steps mempty w) (pure ())  

plan :: (Monoid w,Monad m) => m b -> Plan s w m a b
plan x = Plan mempty (Star (const (lift x))) 

planIO :: (Monoid w,MonadIO m) => IO b -> Plan s w m a b
planIO x = Plan mempty (Star (const (liftIO x))) 

planK :: (Monoid w,Monad m) => (a -> m b) -> Plan s w m a b
planK f = Plan mempty (Star (lift . f)) 

planKIO :: (Monoid w,MonadIO m) => (a -> IO b) -> Plan s w m a b
planKIO f = Plan mempty (Star (liftIO . f)) 

zipSteps' :: Forest a -> Steps r w -> Maybe (Steps (a,r) w)
zipSteps' forest (Steps substeps w) 
    | length forest == length substeps = 
        let paired = Seq.zipWith (\(Node a subforest) (w',e,substeps') -> 
                                        (w',(a,e),zipSteps' subforest substeps'))
                                 (Seq.fromList forest) 
                                 substeps 
        in flip Steps w <$> traverse (\(w',e,ms) -> fmap (\s -> (w',e,s)) ms) paired 
    | otherwise = Nothing

zipSteps :: Forest a -> Plan r w m i o -> Maybe (Plan (a,r) w m i o)
zipSteps forest (Plan steps star) = Plan <$> zipSteps' forest steps <*> pure star 

tickToForest :: Tick c t -> Forest (Maybe (Either t (t,Maybe t)),c)
tickToForest (Tick upwards@(Context completed curr pending :| contexts) progress) = 
    case progress of 
        Skipped forest -> foldr contextToForest (completedToForest completed ++ [Node (Just (Left (getLastTime completed)),curr) (skippedToForest forest (getLastTime completed))] ++ pendingToForest pending) contexts
        Started forest -> foldr contextToForest (pendingToForest forest) upwards
        Finished timeline -> foldr contextToForest (completedToForest completed ++ [Node (Just (Right (getLastTime completed,Just (getLastTime timeline))),curr) (completedToForest timeline)] ++ pendingToForest pending) contexts

simpleTick :: Tick c t -> (NonEmpty (t,c),Progress c t) 
simpleTick (Tick contexts progress) =  (fmap (\(Context previous c _) -> (getLastTime previous,c)) contexts,progress)

contextToForest :: Context c t 
                -> Forest (Maybe (Either t (t,Maybe t)),c)
                -> Forest (Maybe (Either t (t,Maybe t)),c)
contextToForest (Context completed c pending) below =
       completedToForest completed 
    ++ [Node (Just (Right (getLastTime completed,Nothing)),c) below] 
    ++ pendingToForest pending

completedToForest :: Timeline c t -> Forest (Maybe (Either t (t,Maybe t)),c)
completedToForest = undefined

pendingToForest :: Forest c -> Forest (Maybe (Either t (t,Maybe t)),c)
pendingToForest forest = map (fmap (\c -> (Nothing,c))) forest

skippedToForest :: Forest c -> t -> Forest (Maybe (Either t (t,Maybe t)),c)
skippedToForest forest t = map (fmap (\c -> (Just (Left t),c))) forest

-- changeToForest :: Change start end c -> Forest (Maybe (start,Maybe end),c)
-- changeToForest (Started contexts pending) = 
--     foldr contextToForest (pendingToForest pending) contexts 
-- changeToForest (Finished (Context completed (start',current) pending :| contexts) completed end) = 
--     foldr contextToForest [Node (Just (start',Just end),current) (completedToForest completed)] contexts 
-- 
-- contextToForest :: Context start end c -> Forest (Maybe (start,Maybe end),c) -> Forest (Maybe (start,Maybe end),c) 
-- contextToForest (Context completed (start',current) pending) below =
--     completedToForest completed ++ [Node (Just (start',Nothing),current) below] ++ pendingToForest pending
-- 
-- completedToForest :: Forest ((start,end),c) -> Forest (Maybe (start,Maybe end),c) 
-- completedToForest (reverse -> forest) = map (fmap (\((start,end),c) -> (Just (start,Just end),c))) forest
-- 
-- pendingToForest :: Forest c -> Forest (Maybe (start,Maybe end),c) 
-- pendingToForest forest = map (fmap (\c -> (Nothing,c))) forest

unliftPlan :: Monad m => Plan s w m i o -> i -> m o
unliftPlan plan i = snd <$> effects (runPlan (pure ()) plan i)

data Timeline chapter measure = Timeline (Seq (measure,chapter,Either (Forest chapter) (Timeline chapter measure))) measure

getLastTime :: Timeline chapter t -> t
getLastTime (Timeline _ t) = t

data Context c measure = Context
                        {
                          completed :: Timeline c measure
                        , current :: c
                        , pending :: Forest c
                        } 

data Tick c measure = Tick (NonEmpty (Context c measure)) (Progress c measure) 

data Progress c measure = Skipped  (Forest c)
                         | Started (Forest c)
                         | Finished (Timeline c measure)


runPlan :: Monad m 
           => m measure -- ^
           -> Plan s w m a b 
           -> a 
           -> Stream (Of (Tick s measure)) m (Timeline s measure,b)
runPlan makeMeasure (Plan steps (Star f)) initial = 
      let go state stream = 
            do n <- lift (next stream)
               measure <- lift makeMeasure
               case (n,state) of 
                   (Left b,
                    RunState completed [] []) -> do 
                       return (Timeline completed measure,b) 
                   (Right (Skipping_,stream'),
                    RunState previous (Node root subforest:forest) upwards) -> do
                        yield (Tick (Context (Timeline previous measure) root forest :| upwards) 
                                        (Skipped subforest))
                        go (RunState (previous Seq.|> (measure,root,Left subforest)) forest upwards)
                           stream'
                   (Right (Started_,stream'),
                    RunState previous (Node root subforest:forest) upwards) -> do
                        yield (Tick (Context (Timeline previous measure) root forest :| upwards) 
                                        (Started subforest))
                        go (RunState mempty subforest (Context (Timeline previous measure) root forest : upwards))
                           stream'
                   (Right (Finished_,stream'),
                    RunState previous [] (ctx@(Context recap root next) : upwards)) -> do
                        yield (Tick (ctx :| upwards)
                                        (Finished (Timeline previous measure)))
                        go (RunState (previous Seq.|> (measure,root,Right recap)) next upwards)
                           stream'
                   _ -> error "should never happen"
      in go (RunState mempty (stepsToForest steps) []) (f initial)

data RunState c measure = RunState !(Seq (measure,c,Either (Forest c) (Timeline c measure)))
                                   !(Forest c) 
                                   ![Context c measure]

-- TODO Add tickToForest <- working on it
-- TODO Unify recap and timeline? -- leave it for later. Possibly not worth it.
-- TODO Comonad instance for recap and timeline???
-- TODO Some kind of run-in-io function to avoid having to always import streaming  
-- TODO Add "durations :: Timeline -> ..." to use with zipSteps.
-- TODO Add simpleTick.
-- tODO Add timeline folding function.
-- TODO Express Steps and Timeline in terms of Lasanga.
-- TODO Add "mandatoriness" to Steps. Find a better name.
