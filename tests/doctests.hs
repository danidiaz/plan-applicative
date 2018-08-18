module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-ilib","lib/Control/Plan.hs","lib/Control/Plan/Core.hs"]

