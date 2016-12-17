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
                    ,Mandatoriness(..)
                    ,mandatoriness
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
                    ,Control.Comonad.extract
                    ,Data.Bifunctor.bimap
                    ,Data.Bifoldable.bifoldMap
                    ,Data.Bitraversable.bitraverse
                    ,Streaming.hoist
                    ,Streaming.Prelude.effects
                    ,mapTickM
                    ) where

import Prelude hiding ((.),id)
import Data.Tree
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Profunctor (Profunctor(..),Star(..))
import Control.Category
import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Arrow
import Streaming
import Streaming.Prelude

import Control.Plan.Core
