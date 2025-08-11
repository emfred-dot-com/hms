module MathTest (mathTests) where

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
      @?= (Right $ Duration 2)
  , testCase "'1:00 + 1 = 1:01'" $
    eval (case parseExpr "1:00 + 1" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration $ (1 * 60) + 1)
  , testCase "'1 - 1 = 0'" $
      eval (case parseExpr "1 - 1" of
              Left _err -> []
              Right terms -> terms)
      @?= (Right $ Duration 0)
  , testCase "'1:00 - 1 = 0:59'" $
    eval (case parseExpr "1:00 - 1" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration 59)
  , testCase "'1:00:00 - 1 = 0:59:59'" $
    eval (case parseExpr "1:00:00 - 1" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration $ (59 * 60) + 59)
  , testCase "'1:00:00 - 1:00 = 0:59'" $
    eval (case parseExpr "1:00:00 - 1:00" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration (59 * 60))
  , testCase "'1:00:00 - 1:00 - 00:1 = 0:58:59'" $
    eval (case parseExpr "1:00:00 - 1:00 - 00:1" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration $ (58 * 60) + 59)
  , testCase "'0 - 1 = -1'" $
    eval (case parseExpr "0 - 1" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration (-1))
  , testCase "'00:1 * 2 = 00:2'" $
    eval (case parseExpr "00:1 * 2" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration 2)
  , testCase "'1:00 * 2 = 2:00'" $
    eval (case parseExpr "1:00 * 2" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration (2 * 60))
  , testCase "'0:30 * 3 = 1:30'" $
    eval (case parseExpr "0:30 * 3" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration (1 * 60) + 30)
  , testCase "'30:30 * 3 = 1:31:30'" $
    eval (case parseExpr "30:30 * 3" of
             Left _err -> []
             Right terms -> terms)
    @?= (Right $ Duration $ (1 * 60 * 60) + (31 * 60) + 30)
  ]
