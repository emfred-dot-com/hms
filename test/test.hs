module Main (main) where

import Data.Either

import Test.Tasty
import Test.Tasty.HUnit

import Expr
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
  , testCase "10 seconds, '10'" $
      parseExpr "10" @?=
      Right [Dur (S 10)]
  , testCase "10 seconds, '0:10'" $
      parseExpr "0:10" @?=
      Right [Dur (MS 0 10)]
  , testCase "10 seconds, '00:10'" $
      parseExpr "00:10" @?=
      Right [Dur (MS 0 10)]
  , testCase "10 seconds, '0:0:10'" $
      parseExpr "0:0:10" @?=
      Right [Dur (HMS 0 0 10)]
  , testCase "10 seconds, '0:00:10'" $
      parseExpr "0:00:10" @?=
      Right [Dur (HMS 0 0 10)]
  , testCase "10 seconds, '00:0:10'" $
      parseExpr "00:0:10" @?=
      Right [Dur (HMS 0 0 10)]
  , testCase "10 seconds, '00:00:10'" $
      parseExpr "00:00:10" @?=
      Right [Dur (HMS 0 0 10)]
  , testCase "23 minutes, '23:0'" $
      parseExpr "23:0" @?=
      Right [Dur (MS 23 0)]
  , testCase "23 minutes, '23:00'" $
      parseExpr "23:0" @?=
      Right [Dur (MS 23 0)]
  , testCase "23 minutes, '0:23:00'" $
      parseExpr "0:23:0" @?=
      Right [Dur (HMS 0 23 0)]
  , testCase "23 minutes, '00:23:00'" $
      parseExpr "00:23:0" @?=
      Right [Dur (HMS 0 23 0)]
  , testCase "4 hours, '4:0:0'" $
      parseExpr "4:0:0" @?=
      Right [Dur (HMS 4 0 0)]
  , testCase "4 hours, '4:00:0'" $
      parseExpr "4:00:0" @?=
      Right [Dur (HMS 4 0 0)]
  , testCase "4 hours, '4:0:00'" $
      parseExpr "4:0:00" @?=
      Right [Dur (HMS 4 0 0)]
  , testCase "4 hours, '4:00:00'" $
      parseExpr "4:00:00" @?=
      Right [Dur (HMS 4 0 0)]
  ]
