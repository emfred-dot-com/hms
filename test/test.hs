module Main (main) where

import Data.Either

import Test.Tasty
import Test.Tasty.HUnit

import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [parserTests]

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testCase "empty input" $
      assertBool "empty input should fail"
      (isLeft (parseExpr ""))
  ]
