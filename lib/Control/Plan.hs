{-| This module exports the 'Plan' Applicative.

>>> :{
    let example :: Plan String [Int] IO () ()
        example = 
            step "(a)" (step "(a.a)" (foretell [1] *>
                                      plan (threadDelay 1e6)) *>
                        step "(a.a)" (foretell [2] *>
                                      plan (threadDelay 1e6))) *>
            step "(b)" (step "(b.a)" (foretell [3] *>
                                      plan (threadDelay 1e6)) *>
                        step "(b.b)" (foretell [4] *>
                                      plan (threadDelay 1e6)))
    in 
    bifoldMap id (foldMap Prelude.show) (getSteps example)
:}
"(a)(a.a)1(a.a)2(b)(b.a)3(b.b)4"
-}
module Control.Plan (
                    -- * Constructing plans
                     Plan
                    ,plan
                    ,planIO
                    ,planK
                    ,planKIO
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
                    ,tickToForest
                    ,Tick(..)
                    ,Context(..)
                    ,Progress(..)
                    ,Timeline
                    ,instants
                    ,foldTimeline
                    -- ** Running arrow plans
                    ,unliftPlanK
                    ,runPlanK
                    -- * The Lasagna typeclass
                    ,Lasagna(..)
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
