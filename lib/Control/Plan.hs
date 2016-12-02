{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
module Control.Plan (Plan
                    ,Steps(..)
                    ,Tick(..)
                    ,getSteps
                    ,runPlan
                    ,step
                    ,foretell
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


-- TODO:
-- foldSteps catamorphism
-- stepsToTree
-- some kind of run-in-io function to avoid having to always import streaming  
-- ArrowChoice instance? 
