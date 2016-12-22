-- | Prefer using the main module. If you  manipulate the internals of `Plan`
-- to add fake steps, bad things might happen.

{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language ViewPatterns #-}
{-# language NamedFieldPuns #-}
module Control.Plan.Core (module Control.Plan.Core) where

import Prelude hiding ((.),id)
import qualified Data.Bifunctor as Bifunctor
import Data.Foldable
import Data.Bifoldable
import Data.Bitraversable
import Data.Bifunctor(Bifunctor,bimap)
import Data.Bifunctor.Clown
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Tree
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Profunctor (Profunctor(..),Star(..))
import Control.Category
import Control.Arrow
import Control.Monad
import Control.Comonad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Streaming (hoist)
import qualified Streaming.Prelude
import Streaming.Prelude (Stream,Of(..),yield,next,effects)

-- | A computation that takes inputs of type @i@ and produces outputs of type
-- @o@ working in the underlying monad @m@. The 'Applicative' instance cares
-- only about the outputs, the 'Arrow' instance cares about both inputs and
-- outputs.
--
-- Parts of the computation can be labeled as steps with tags of type @s@. 
--
-- Computations can have monoidal resource annotations of type @w@.
--
-- The structure of steps and the monoidal annotations can be inspected before
-- executing the computations.
data Plan s w m i o = Plan (Steps s w) (Star (Stream (Of Tick') m) i o) deriving Functor

instance (Monoid w,Monad m) => Applicative (Plan s w m i) where
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

-- | A 'Data.Tree.Forest' of steps tags of type @s@ interspersed with monoidal
-- annotations of type @w@.
data Steps s w = Steps !(Seq (w,s,Mandatoriness,Steps s w)) w 
               deriving (Functor,Foldable,Traversable,Eq,Show)

-- | Steps of 'Plan's constructed in 'Applicative' fashion are always
-- 'Mandatory'. Only steps declared with 'skippable' are optional.
data Mandatoriness = Skippable
                   | Mandatory
                   deriving (Show,Eq,Ord)

instance Bifunctor Steps where
    first f (Steps steps w) = 
        let go (w',e,mandatoriness',substeps) = (w',f e,mandatoriness',Bifunctor.first f substeps) 
        in  Steps (fmap go steps) w
    second = fmap

-- | 'bifoldMap' allows extracting the steps and the annotations together. 
--
instance Bifoldable Steps where
    bifoldMap g f (Steps steps w) = 
        foldMap (\(w',s,_,substeps) -> f w'
                                       `mappend` 
                                       g s 
                                       `mappend` 
                                       bifoldMap g f substeps) steps
        `mappend`
        f w

instance Bitraversable Steps where
    bitraverse g f (Steps steps w) = 
        Steps <$> traverse innertraverse steps <*> f w
        where
        innertraverse (w',e,mandatoriness',substeps) = 
            (,,,) <$> f w' <*> g e <*> pure mandatoriness' <*> bitraverse g f substeps
    
instance Monoid w => Monoid (Steps s w) where
    mempty = Steps mempty mempty
    Steps s1 w1 `mappend` Steps s2 w2 = 
        case Seq.viewl s2 of
            Seq.EmptyL -> Steps s1 (w1 `mappend` w2)
            (w',s,mandatoriness',substeps) Seq.:< s2' -> 
                Steps (s1 `mappend` ((w1 `mappend` w',s,mandatoriness',substeps) Seq.<| s2')) w2

-- | A catamorphism on 'Step's, that "destroys" the 'Step' value from the
-- leaves upwards.
--
-- Unlike 'foldMap' or 'bifoldMap', it allows a more structured analysis of the
-- annotations, by preserving their relationship with the hierarchy of steps.
--
foldSteps :: ([(w,s,Mandatoriness,r)] -> w -> r) -- ^ A function that consumes a list of step tags of type @s@, surrounded and interleaved with annotations of type @w@. Each step is also annotated with its mandatoriness and with the result @r@ of consuming its substeps, if there were any.
          -> Steps s w 
          -> r
foldSteps f = foldSteps' (\steps -> f (toList steps))

foldSteps' :: (Seq (w,s,Mandatoriness,r) -> w -> r) -> Steps s w -> r
foldSteps' f = go
    where
    go (Steps steps w) = 
        f (fmap (\(w',e',mandatoriness',substeps) -> (w',e',mandatoriness',go substeps)) steps) w

-- | Adapt the 'Step' value inside a 'Plan' without extracting it.
bimapSteps ::  (s -> s') -> (w -> w') -> Plan s w m i o -> Plan s' w' m i o
bimapSteps f g (Plan steps star) = Plan (Bifunctor.bimap f g steps) star

-- | Use a lens setter to "zoom" the monoidal annotations of a 'Plan' into a
-- wider monoidal context.
zoomSteps :: Monoid w' => ((w -> Identity w) -> w' -> Identity w') -> Plan s w m i o -> Plan s w' m i o
zoomSteps setter = bimapSteps id (\w -> set' w mempty)
    where
    set' w = runIdentity . setter (Identity . const w)

-- | Change the underlying monad of a 'Plan'.
hoistPlan :: Monad m => (forall x. m x -> n x) -> Plan s w m i o -> Plan s w n i o
hoistPlan trans (Plan steps (Star f)) = Plan steps (Star (hoist trans . f)) 

data Tick' = Skipped' | Started' | Finished' deriving (Eq,Ord,Enum,Show)

-- | Inspect a plan without executing it.
getSteps :: Plan s w m i o -> Steps s w
getSteps (Plan steps _) = steps

-- | Decorate each step tag with its mandatoriness. Useful in combination with 'toForest'.
mandatoriness :: Steps s w -> Steps (Mandatoriness,s) w
mandatoriness (Steps steps w) = Steps (fmap go steps) w
    where
    go (w',s,mandatory,substeps) = (w',(mandatory,s),mandatory,mandatoriness substeps)

-- | Declare a step by wrapping an existing plan (which may contain substeps).
step :: (Monoid w,Monad m) => s -> Plan s w m i o -> Plan s w m i o
step s (Plan forest (Star f)) = 
    Plan (Steps (Seq.singleton (mempty,s,Mandatory,forest)) mempty) 
         (Star (\x -> yield Started' *> f x <* yield Finished'))

-- | Declare an optional step by wrapping an existing arrow plan. The step will
-- only be executed when the input is 'Just'.
--
-- This function only makes sense when using the 'Arrow' instance of 'Plan',
-- because for 'Applicative's an effect cannot depend on previously obtained
-- values.
skippable :: (Monoid w,Monad m) => s -> Plan s w m i o -> Plan s w m (Maybe i) ()
skippable s (Plan forest (Star f)) = 
    Plan (Steps (Seq.singleton (mempty,s,Skippable,forest)) mempty) 
         (Star (\m -> case m of
                        Just x -> yield Started' *> f x *> yield Finished'
                        Nothing -> yield Skipped'))

-- | Declare a monoidal annotation. The annotation can be later inspected
-- without having to run the 'Plan'.
--
-- Usually the annotations will represent resources that the 'Plan' is expected
-- to require.
foretell :: (Monad m) => w -> Plan s w m i ()
foretell w = Plan (Steps mempty w) (pure ())  

-- | Lift a monadic action to a 'Plan'. The input type remains polymorphic.
plan :: (Monoid w,Monad m) => m o -> Plan s w m i o
plan x = Plan mempty (Star (const (lift x))) 

-- | Lift an 'IO' action to a 'Plan'. The input type remains polymorphic.
planIO :: (Monoid w,MonadIO m) => IO o -> Plan s w m i o
planIO x = Plan mempty (Star (const (liftIO x))) 

-- | Lift a Kleisli arrow to a 'Plan'.
planK :: (Monoid w,Monad m) => (i -> m o) -> Plan s w m i o
planK f = Plan mempty (Star (lift . f)) 

-- | Lift a Kleisli arrow working in 'IO' to a 'Plan'.
planKIO :: (Monoid w,MonadIO m) => (i -> IO o) -> Plan s w m i o
planKIO f = Plan mempty (Star (liftIO . f)) 

zipSteps' :: Forest a -> Steps r w -> Maybe (Steps (a,r) w)
zipSteps' forest (Steps substeps w) 
    | length forest == length substeps = 
        let paired = Seq.zipWith (\(Node a subforest) (w',s,mandatory,substeps') -> 
                                        (w',(a,s),mandatory,zipSteps' subforest substeps'))
                                 (Seq.fromList forest) 
                                 substeps 
            go (w',s,mandatory,ms) = fmap (\x -> (w',s,mandatory,x)) ms
        in  flip Steps w <$> traverse go paired 
    | otherwise = Nothing

-- | Pair each step tag @s@ inside a 'Plan' with the corresponding element of the 'Forest'.
--
-- If the forest doesn't have the same structure as the steps, the function
-- fails with 'Nothing'.
--
-- This function can be useful to annotate each step tag with some information,
-- for example the time duration of the step in a previous execution of the
-- plan. See 'Timeline', 'instants', and 'toForest'.
zipSteps :: Forest s' -> Plan s w m i o -> Maybe (Plan (s',s) w m i o)
zipSteps forest (Plan steps star) = Plan <$> zipSteps' forest steps <*> pure star 

-- | Transform a 'Tick' into a form more suitable for rendering with functions
-- like 'Data.Tree.drawForest'.
--
-- A given step might not have been reached yet. It it has been reached, either
-- it has been skipped at a certain time, or started at a certain time. If if
-- has been started, maybe it has already finised, too.
tickToForest :: Tick s t -> Forest (Maybe (Either t (t,Maybe t)),s)
tickToForest (Tick upwards@(Context {completed,current,pending}:|contexts) progress) = 
    case progress of 
        Skipped forest -> foldl contextToForest 
                                ( completedToForest completed 
                                  ++ 
                                  [Node (Just (Left (extract completed))
                                        ,current) 
                                        (skippedToForest forest (extract completed))] 
                                  ++
                                  pendingToForest pending ) 
                                contexts
        Started forest -> foldl contextToForest 
                                (pendingToForest forest) 
                                upwards
        Finished timeline -> foldl contextToForest 
                                   ( completedToForest completed 
                                     ++ 
                                     [Node (Just (Right (extract completed,Just (extract timeline)))
                                           ,current) 
                                           (completedToForest timeline)] 
                                     ++ pendingToForest pending ) 
                                   contexts

contextToForest :: Forest (Maybe (Either t (t,Maybe t)),s)
                -> Context s t 
                -> Forest (Maybe (Either t (t,Maybe t)),s)
contextToForest below (Context {completed,current,pending}) =
       completedToForest completed 
    ++ [Node (Just (Right (extract completed,Nothing)),current) below] 
    ++ pendingToForest pending

completedToForest :: Timeline c t -> Forest (Maybe (Either t (t,Maybe t)),c)
completedToForest (toForest . instants -> forest) = fmap (fmap go) forest
    where
    go = Bifunctor.first (Just . bimap id (fmap Just))

pendingToForest :: Forest c -> Forest (Maybe (Either t (t,Maybe t)),c)
pendingToForest forest = map (fmap (\c -> (Nothing,c))) forest

skippedToForest :: Forest c -> t -> Forest (Maybe (Either t (t,Maybe t)),c)
skippedToForest forest t = map (fmap (\c -> (Just (Left t),c))) forest

-- | Forget that there is a plan, get the underlying monadic action.
unliftPlan :: Monad m => Plan s w m () o -> m o
unliftPlan p = extract <$> effects (runPlanK (pure ()) p ())

-- | Forget that there is a plan, get the underlying Kleisli arrow.
unliftPlanK :: Monad m => Plan s w m i o -> i -> m o
unliftPlanK p i = extract <$> effects (runPlanK (pure ()) p i)

-- | A 'Data.Tree.Forest' of steps tags of type @s@ interspersed with
-- measurements of type @t@.
data Timeline s t = Timeline !(Seq (t,s,Either (Forest s) (Timeline s t))) t 
                  deriving (Functor,Foldable,Traversable,Eq,Show)

instance Bifunctor Timeline where
    first f (Timeline steps w) = 
        let go (w',e,substeps) = (w',f e,bimap (fmap (fmap f)) (Bifunctor.first f) substeps) 
        in  Timeline (fmap go steps) w
    second = fmap

instance Bifoldable Timeline where
    bifoldMap g f (Timeline steps w) = 
        foldMap (\(w',e,substeps) -> f w'
                                  `mappend` 
                                  g e 
                                  `mappend` 
                                  bifoldMap (mconcat . map (foldMap g)) (bifoldMap g f) substeps) steps
        `mappend`
        f w

instance Bitraversable Timeline where
    bitraverse g f (Timeline steps w) = 
        Timeline <$> traverse innertraverse steps <*> f w
        where
        innertraverse (w',e,substeps) = (,,) 
                                    <$> f w' 
                                    <*> g e 
                                    <*> bitraverse (traverse (traverse g)) (bitraverse g f) substeps

-- | 'Timeline's always have at least one measurement. 'extract' gives the final measurement.
instance Comonad (Timeline s) where
    extract (Timeline _ t) = t
    duplicate tip@(Timeline steps _) = 
        let go steps' = case Seq.viewr steps' of  
                Seq.EmptyR -> error "should never happen"
                lefto Seq.:> (t',c',timeline') -> ((Timeline lefto t'),c',fmap duplicate timeline')
        in Timeline (fmap go (Seq.inits steps)) tip

-- | Decorate each step tag with either the time the step was skipped, or the
-- time it was started and finished. Useful in combination with 'toForest'.
instants :: Timeline s t -> Timeline (Either t (t,t),s) t
instants (Timeline past limit) = Timeline (fmap go past) limit
    where
    go (t',c',Left forest)     = (t',(Left  t',c')                    ,Left  (fmap (fmap (\x -> (Left t',x))) forest))
    go (t',c',Right timeline') = (t',(Right (t',extract timeline'),c'),Right (instants timeline'))

-- | A catamorphism on 'Timeline's, that "destroys" the 'Timeline' value from the
-- leaves upwards.
--
foldTimeline :: ([(t,s,Either (Forest s) r)] -> t -> r) -- ^ A function that consumes a list of step tags of type @s@, surrounded and interleaved with measurements of type @t@. Each step is also annotated with either its substeps, if it the step was skipped, or the results of consuming the substeps, if it was executed.
             -> Timeline s t 
             -> r
foldTimeline f = foldTimeline' (\steps -> f (toList steps))
    
foldTimeline' :: (Seq (t,c,Either (Forest c) r) -> t -> r) -> Timeline c t -> r
foldTimeline' f = go
    where
    go (Timeline steps t) = f (fmap (\(t',c',foreste) -> (t',c',fmap go foreste)) steps) t

-- | Represents how far we are along a sequence of sibling steps.
--
-- For the already completed steps, a 'Timeline' of measurements is provided. 'extract' for the 'Timeline' returns the starting measurement of the current step.
data Context s t = Context
                 {
                   completed :: Timeline s t
                 , current :: s
                 , pending :: Forest s
                 } deriving (Functor,Foldable,Traversable,Eq,Show) 

instance Bifunctor Context where
    first  f (Context {completed,current,pending}) =  
                Context (Bifunctor.first f completed) 
                        (f current) 
                        (fmap (fmap f) pending)
    second = fmap

-- | Represents some kind of progress through the 'Steps' of a 'Plan' while the
-- plan executes.
-- 
-- The ascending list of contexts provides the current position of the
-- execution along the hierarchy of steps.
--
-- If the plan only has a linear sequence of steps, the list will have only one
-- 'Context'.
data Tick s t = Tick (NonEmpty (Context s t)) (Progress s t) 
                deriving (Functor,Foldable,Traversable,Eq,Show) 

instance Bifunctor Tick where
    first f (Tick contexts progress) = 
                Tick (fmap (Bifunctor.first f) contexts) (Bifunctor.first f progress)
    second = fmap

-- | The execution of a 'Plan' can make progress by skipping a step, starting a
-- step, or finishing a step.
data Progress s t = Skipped  (Forest s) -- ^ Provides the substeps that were skipped.
                  | Started (Forest s) -- ^ Provides the substeps that will be executed next.
                  | Finished (Timeline s t) -- ^ Provides a 'Timeline' of measurements for the completed substeps. 'extract' for the 'Timeline' gives the finishing measurement for the current step.
                    deriving (Functor,Foldable,Traversable,Eq,Show) 

instance Bifunctor Progress where
    first f (Skipped forest) = Skipped (fmap (fmap f) forest)
    first f (Started forest) = Skipped (fmap (fmap f) forest)
    first f (Finished timeline) = Finished (bimap f id timeline)
    second = fmap

-- | Specify a monadic callback for processing each 'Tick' update.
onTick :: Monad m => (tick -> m ()) -> Stream (Of tick) m r -> m r
onTick = Streaming.Prelude.mapM_

-- | Runs a plan that doesn't need input. It returns a 'Stream' of 'Tick'
-- updates that are emitted every time the execution advances through the
-- 'Steps'. 
--
-- For each 'Tick' update, a monadic measurement of type @t@ is taken. Usually
-- the measurement consists in getting the current time.
--
-- When the execution finishes, a 'Timeline' with the measurements for each
-- 'Tick' is returned, along with the result value. 
--
-- Even if the plan didn't have any steps, the 'Timeline' will contain a
-- measurement taken when the computation finished.
runPlan :: Monad m 
        => m t -- ^ Monadic measurement to be taken on each tick.
        -> Plan s w m () o -- ^ Plan without input.
        -> Stream (Of (Tick s t)) m (Timeline s t,o) 
runPlan measurement p = runPlanK measurement p () 

-- | Like 'runPlan', but for 'Arrow'-like 'Plan's that take inputs.
runPlanK :: Monad m 
         => m t -- ^ Monadic measurement to be taken on each tick.
         -> Plan s w m i o -- ^ Plan that takes input.
         -> i 
         -> Stream (Of (Tick s t)) m (Timeline s t,o)
runPlanK makeMeasure (Plan steps (Star f)) initial = 
      let go state stream = 
            do n <- lift (next stream)
               measure <- lift makeMeasure
               case (n,state) of 
                   (Left b,
                    RunState completed [] []) -> do 
                       return (Timeline completed measure,b) 
                   (Right (Skipped',stream'),
                    RunState previous (Node root subforest:forest) upwards) -> do
                        yield (Tick (Context (Timeline previous measure) root forest :| upwards) 
                                    (Skipped subforest))
                        go (RunState (previous Seq.|> (measure,root,Left subforest)) forest upwards)
                           stream'
                   (Right (Started',stream'),
                    RunState previous (Node root subforest:forest) upwards) -> do
                        yield (Tick (Context (Timeline previous measure) root forest :| upwards) 
                                    (Started subforest))
                        go (RunState mempty subforest (Context (Timeline previous measure) root forest : upwards))
                           stream'
                   (Right (Finished',stream'),
                    RunState previous' [] (ctx@(Context {completed,current,pending}) : upwards)) -> do
                        let subtimeline = Timeline previous' measure
                            Timeline previous'' instant = completed
                        yield (Tick (ctx :| upwards)
                                    (Finished subtimeline))
                        go (RunState (previous'' Seq.|> (instant,current,Right subtimeline)) pending upwards)
                           stream'
                   _ -> error "should never happen"
      in go (RunState mempty (toForest steps) []) (f initial)

data RunState s t = RunState !(Seq (t,s,Either (Forest s) (Timeline s t)))
                             !(Forest s) 
                             ![Context s t]

-- | Instances of 'Lasagna' are like 'Data.Tree.Forest's where each list of
-- sibling nodes of type @n@ is surrounded and interspersed with annotations of
-- type @a@. Some instances might add extra information to each node, or
-- allow alternative branches.
class (Bitraversable l) => Lasagna l where
    -- | Substitute each node with the ascending path towards its topmost
    -- parent.
    paths    :: l n a -> l (NonEmpty n) a 
    -- | Forget about the annotations and return the underlying 'Data.Tree.Forest'.
    toForest :: l n a -> Forest n

-- | 'toForest' forgets about the annotations and returns a 'Forest' of step
-- tags.
instance Lasagna Steps where
    paths steps = 
        let algebra ws r acc = Steps (fmap (downwards acc) ws) r  
            downwards acc (w',s',mandatoriness',func) = (w',s':|acc,mandatoriness',func (s':acc))
        in foldSteps' algebra steps []
    toForest (Steps steps _) = 
        map (\(_,e,_,steps') -> Node e (toForest steps')) (toList steps) 

-- | 'toForest' forgets about the measurements and returns a 'Forest' of step
-- tags.
instance Lasagna Timeline where
    paths steps = 
        let algebra ws r acc = Timeline (fmap (downwards acc) ws) r  
            downwards acc (w',s',funce) = (w',s':|acc,bimap (fmap (inheritTree (s':acc))) (\f -> f (s':acc)) funce)
        in foldTimeline' algebra steps []
    toForest (Timeline past _) = fmap (\(_,c,timeline') -> Node c (either id toForest timeline')) (toList past)

-- | A 'Data.Tree.Forest' is a 'Lasagna' for which no annotations exist.
instance Lasagna (Clown (Compose [] Tree)) where
    paths (Clown (Compose forest)) = (Clown (Compose (fmap (inheritTree []) forest)))
    toForest (Clown (Compose forest)) = forest 

inheritTree :: [a] -> Tree a -> Tree (NonEmpty a)
inheritTree acc tree = foldTree' algebra tree acc where
    algebra :: a -> [[a] -> Tree (NonEmpty a)] -> [a] -> Tree (NonEmpty a)
    algebra a fs as = Node (a:|as) (fs <*> [a:as]) 

-- | A tree catamorphism. This function already exists in the latest version of
-- "containers"
foldTree' :: (a -> [b] -> b) -> Tree a -> b
foldTree' f = go where
    go (Node x ts) = f x (map go ts)

