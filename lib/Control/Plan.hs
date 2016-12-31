{-| This module exports the 'Plan' Applicative.

>>> :{
    let example :: Plan String [Int] IO () ()
        example = 
            step "a" (step "b" (foretell [1] *> plan (threadDelay 1e6)) 
                      *>
                      step "c" (foretell [2] *> plan (threadDelay 1e6))) 
            *>
            step "d" (step "e" (foretell [3] *> plan (threadDelay 1e6)) 
                      *>
                      step "f" (foretell [4] *> plan (threadDelay 1e6)))
    in 
    bifoldMap id (foldMap Prelude.show) (getSteps example)
:}
"ab1c2de3f4"

Some possible use cases:

- Inspect the steps of an existing 'Plan' from @ghci@ using 'getSteps',
  'toForest' and 'Data.Tree.drawForest', as a form of documentation.

- If your script requires files that must be already present in the file
  system, use 'foretell' to annotate each 'Plan' action that requires a file,
  then get the global list of files using 'getSteps' and 'foldMap', and check
  that they all exist before running the 'Plan' with 'runPlan'.

- Get progress updates for your script by declaring (possibly nested) steps
  with 'step', running the 'Plan' with 'runPlan', and providing a notification
  callback with 'onTick', probably using 'completedness','toForest' and
  'Data.Tree.drawForest' to render the updates.

- Run a 'Plan' with 'runPlan', use 'instants' an 'toForest' on the resulting
  'Timeline' to get the durations of each step, then use 'zipSteps' on the same
  'Plan' and run it again. Now whenever a step finishes we can know if it took
  more or less than in the previous execution.

-}
module Control.Plan (
                    -- * Constructing plans
                     Plan
                    ,plan
                    ,planIO
                    ,kplan
                    ,kplanIO
                    -- ** Declaring steps and annotations
                    ,step
                    ,skippable
                    ,foretell
                    -- * Analyzing plans
                    ,getSteps
                    ,Steps
                    ,mandatoriness
                    ,Mandatoriness(..)
                    ,foldSteps
                    -- * Adapting plans
                    -- $adapting
                    ,bimapSteps
                    ,zoomSteps
                    ,zipSteps
                    ,hoistPlan
                    -- * Running plans
                    ,unliftPlan
                    ,runPlan
                    ,onTick
                    ,Tick(..)
                    ,completedness
                    ,Context(..)
                    ,Progress(..)
                    ,Timeline
                    ,instants
                    ,foldTimeline
                    -- ** Running arrow plans
                    ,unliftKPlan
                    ,runKPlan
                    -- * The Sylvan typeclass
                    ,Sylvan(..)
                    -- * Re-exports
                    ,Data.Bifunctor.bimap
                    ,Data.Bifoldable.bifoldMap
                    ,Data.Bitraversable.bitraverse
                    ,Control.Comonad.extract
                    -- $extract
                    ,Streaming.hoist
                    ,Streaming.Prelude.effects
                    -- $effects
                    ) where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Control.Comonad
import Streaming
import Streaming.Prelude

import Control.Plan.Core

{- $setup

>>> :set -XNumDecimals
>>> :set -XArrows
>>> import Control.Applicative
>>> import Control.Plan
>>> import Control.Concurrent(threadDelay)

-}

{- $adapting

   Sometimes, we might need to mix 'Plan's for which step tags and annotations
   are of different types. These functions help with that.

-}

{- $extract
   Besides its usefulness with 'Timeline', 'extract' lets you get the head of a
   'NonEmpty' or the second element of a tuple.
-}

{- $effects
   'effects' lets you ignore all the update notifications while running a plan,
   when you are only interested in the final 'Timeline' and the result.
-}
