-- | This module exports the 'Plan' Applicative.

{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
module Control.Plan (Plan
                    ,Steps(..)
                    ,Tick(..)
                    -- * Analyzing and running plans
                    ,getSteps
                    ,stepsToForest
                    ,runPlan
                    -- * Declaring steps and annotations
                    ,step
                    ,foretell
                    -- * Constructing plans
                    ,plan
                    ,planIO
                    ,planK
                    ,planKIO
                    ) where

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

import Control.Plan.Core
