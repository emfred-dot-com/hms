module Main (main) where

import Test.Tasty

import ParserTest
import MathTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ parserTests
  , mathTests
  ]

