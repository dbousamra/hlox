module Main where

import           ParsingSpec as ParsingSpec
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
    ParsingSpec.test
  ]

