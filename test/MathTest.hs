module MathTest (mathTests) where

import Data.Either

import Test.Tasty
import Test.Tasty.HUnit

import Expr
import Parser
import Eval

mathTests :: TestTree
mathTests = testGroup "Math"
  [ testCase "'1 + 1 = 2'" $
      eval (case parseExpr "1 + 1" of
              Left _err -> []
              Right terms -> terms)
      @?= Right (HMS 0 0 2)
  , testCase "'1:00 + 1 = 1:01'" $
    eval (case parseExpr "1:00 + 1" of
             Left _err -> []
             Right terms -> terms)
    @?= Right (HMS 0 1 1)
  , testCase "'1 - 1 = 0'" $
      eval (case parseExpr "1 - 1" of
              Left _err -> []
              Right terms -> terms)
      @?= Right (HMS 0 0 0)
  , testCase "'1:00 - 1 = 0:59'" $
    eval (case parseExpr "1:00 - 1" of
             Left _err -> []
             Right terms -> terms)
    @?= Right (HMS 0 0 59)
  , testCase "'1:00:00 - 1 = 0:59:59'" $
    eval (case parseExpr "1:00:00 - 1" of
             Left _err -> []
             Right terms -> terms)
    @?= Right (HMS 0 59 59)
  , testCase "'1:00:00 - 1:00 = 0:59'" $
    eval (case parseExpr "1:00:00 - 1:00" of
             Left _err -> []
             Right terms -> terms)
    @?= Right (HMS 0 59 0)
  , testCase "'1:00:00 - 1:00 - 00:1 = 0:58:59'" $
    eval (case parseExpr "1:00:00 - 1:00 - 00:1" of
             Left _err -> []
             Right terms -> terms)
    @?= Right (HMS 0 58 59)
  , testCase "'0 - 1 = -1'" $
    eval (case parseExpr "0 - 1" of
             Left _err -> []
             Right terms -> terms)
    @?= Right (HMS 0 0 (-1))
  ]
