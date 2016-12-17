-- | This module exports the 'Plan' Applicative.

{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language PatternSynonyms #-}
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
                    ,bimapSteps
                    ,zoomSteps
                    ,zipSteps
                    ,hoistPlan
                    -- * Running plans
                    ,runPlan
                    ,tickToForest
                    ,Tick(..)
                    ,Context(..)
                    ,Progress(..)
                    ,Timeline
                    ,instants
                    ,foldTimeline
                    ,unliftPlan
                    -- * The Lasagna typeclass
                    ,Lasagna(..)
                    -- * Re-exports
                    ,Data.Bifunctor.bimap
                    ,Data.Bifoldable.bifoldMap
                    ,Data.Bitraversable.bitraverse
                    ,Control.Comonad.extract
                    ,Streaming.hoist
                    ,mapTickM
                    ,Streaming.Prelude.effects
                    ) where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Control.Comonad
import Streaming
import Streaming.Prelude

import Control.Plan.Core
