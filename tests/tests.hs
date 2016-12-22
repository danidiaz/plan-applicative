{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding ((.),id)
import Data.Monoid
import Data.Foldable
import Data.Tree

import Control.Category
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Comonad

import Control.Plan
import Control.Plan.Core

import Test.Tasty
import Test.Tasty.HUnit

import Streaming
import qualified Streaming.Prelude

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testCase "simple" testSimple
                          ,testCase "multi" testMulti
                          ,testCase "pathsMulti" testPathsMulti
                          ,testCase "runMulti" testRunMulti
                          ,testCase "skippy" testSkippy
                          ,testCase "runSkippy" testRunSkippy
                          ]

testSimple :: IO ()
testSimple = do
    let plan' = pure 7 :: Plan Char () IO () Int
    assertEqual "" []
                   (bifoldMap pure (const []) (getSteps plan')) 

multi :: Plan String [Int] (Writer [String]) () ()
multi = do
    _ <- step "a" (do _ <- step "a1" (foretell [1] *> plan (tell ["a1"]) <* foretell [2])
                      _ <- step "a2" (foretell [3] *> plan (tell ["a2"]) <* foretell [4])
                      return ())
    _ <- step "b" (do _ <- step "b1" (foretell [5] *> plan (tell ["b1"]) <* foretell [6])
                      _ <- step "b2" (foretell [7] *> plan (tell ["b2"]) <* foretell [8])
                      return ())
    return ()

testMulti :: IO ()
testMulti = assertEqual "" [Left "a"
                           ,Left "a1"
                           ,Right 1
                           ,Right 2
                           ,Left "a2"
                           ,Right 3
                           ,Right 4
                           ,Left "b"
                           ,Left "b1"
                           ,Right 5
                           ,Right 6
                           ,Left "b2"
                           ,Right 7
                           ,Right 8
                           ]
                           (bifoldMap (pure . Left) (map Right) . getSteps $ multi)

testPathsMulti :: IO ()
testPathsMulti = assertEqual "" [["a"],["a1","a"],["a2","a"],["b"],["b1","b"],["b2","b"]]
                                (map toList . bifoldMap pure (const []) . paths . getSteps $ multi)

progressToTick' :: Progress s t -> Tick' 
progressToTick' (Skipped {}) = Skipped'
progressToTick' (Started {}) = Started'
progressToTick' (Finished {}) = Finished'

testRunMulti :: IO ()
testRunMulti = do
      let multi' = hoistPlan lift multi 
          addToCounter = modify' succ >> get
          ((ticks :> (timeline,_),_),results) = runWriter 
                                              . flip runStateT 'a'
                                              . Streaming.Prelude.toList 
                                              . runPlan addToCounter $ multi' 
      assertEqual "timeline" [Node (Right ('b','g'),"a") [Node (Right ('c','d'),"a1") []
                                                         ,Node (Right ('e','f'),"a2") []]
                             ,Node (Right ('h','m'),"b") [Node (Right ('i','j'),"b1") []
                                                         ,Node (Right ('k','l'),"b2") []]]
                             (toForest (instants timeline))
      assertEqual "timelineEnd" 'n' 
                                (extract timeline)
      assertEqual "ticksLen" 12 
                            (length ticks) 
      let simpleTicks = map (\(Tick ctxs progress) -> (toList . fmap (extract.completed) $ ctxs
                                                      ,toList . fmap current $ ctxs
                                                      ,progressToTick' progress)) 
                            ticks
      assertEqual "tickTypes" [("b" ,["a"],Started')
                              ,("cb",["a1","a"],Started')
                              ,("cb",["a1","a"],Finished')
                              ,("eb",["a2","a"],Started')
                              ,("eb",["a2","a"],Finished')
                              ,("b" ,["a"],Finished')
                              ,("h" ,["b"],Started')
                              ,("ih",["b1","b"],Started')
                              ,("ih",["b1","b"],Finished')
                              ,("kh",["b2","b"],Started')
                              ,("kh",["b2","b"],Finished')
                              ,("h" ,["b"],Finished')
                              ]
                              simpleTicks
      let forestTicks = take 3 . map tickToForest $ ticks
      assertEqual "tickForests" [[Node (Just (Right ('b',Nothing)),"a") [Node (Nothing,"a1") []
                                                                        ,Node (Nothing,"a2") []]
                                 ,Node (Nothing,"b") [Node (Nothing,"b1") []
                                                     ,Node (Nothing,"b2") []]]
                                ,[Node (Just (Right ('b',Nothing)),"a") [Node (Just (Right ('c',Nothing)),"a1") []
                                                                        ,Node (Nothing,"a2") []]
                                 ,Node (Nothing,"b") [Node (Nothing,"b1") []
                                                     ,Node (Nothing,"b2") []]]

                                ,[Node (Just (Right ('b',Nothing)),"a") [Node (Just (Right ('c',Just 'd')),"a1") []
                                                                        ,Node (Nothing,"a2") []]
                                 ,Node (Nothing,"b") [Node (Nothing,"b1") []
                                                     ,Node (Nothing,"b2") []]]]
                                forestTicks

skippy :: Plan String [Int] (Writer [String]) () ()
skippy = step "a" (plan (return (Just ())))
         >>>
         skippable "sa" (plan (tell ["sa"]))
         >>>
         step "b" (plan (return Nothing))
         >>>
         skippable "sb" (plan (tell ["sb"]))

testSkippy :: IO ()
testSkippy = assertEqual "" [Left (Mandatory,"a")
                            ,Left (Skippable,"sa")
                            ,Left (Mandatory,"b")
                            ,Left (Skippable,"sb")
                            ]
                            (bifoldMap (pure . Left) (map Right) . mandatoriness . getSteps $ skippy)

testRunSkippy :: IO ()
testRunSkippy = do
      let skippy' = hoistPlan lift skippy
          addToCounter = modify' succ >> get
          ((ticks :> (timeline,_),_),results) = runWriter 
                                              . flip runStateT 'a'
                                              . Streaming.Prelude.toList 
                                              . runPlan addToCounter $ skippy' 
      assertEqual "timeline" [Node (Right ('b','c'),"a") []
                             ,Node (Right ('d','e'),"sa") []
                             ,Node (Right ('f','g'),"b") []
                             ,Node (Left 'h',"sb") []
                             ]
                             (toForest (instants timeline))
      assertEqual "timelineEnd" 'i' 
                                (extract timeline)
      assertEqual "ticksLen" 7
                             (length ticks) 
      let simpleTicks = map (\(Tick ctxs progress) -> (toList . fmap (extract.completed) $ ctxs
                                                      ,toList . fmap current $ ctxs
                                                      ,progressToTick' progress)) 
                            ticks
      assertEqual "tickTypes" [("b",["a"],Started')
                              ,("b",["a"],Finished')
                              ,("d",["sa"],Started')
                              ,("d",["sa"],Finished')
                              ,("f",["b"],Started')
                              ,("f",["b"],Finished')
                              ,("h",["sb"],Skipped')]
                              simpleTicks
      let forestTicks = take 1 . map tickToForest $ ticks
      assertEqual "tickForests" [[Node (Just (Right ('b',Nothing)),"a") []
                                 ,Node (Nothing,"sa") []
                                 ,Node (Nothing,"b") []
                                 ,Node (Nothing,"sb") []]]
                                forestTicks
