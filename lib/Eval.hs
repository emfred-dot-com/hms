module Eval
  ( eval
  )
where

import Duration
import Expr

type EvalError = String

eval :: Expr -> Either EvalError Duration
eval expr =
  case expr of
    -- []
    [] ->
      Left "Empty expression."
    -- [dur, ...]
    Dur d : rest ->
      case rest of
        -- [dur]
        [] ->
          Right (normalize d)
        -- [dur, dur, ...]
        Dur d' : _rest' ->
          Left ("Operator missing between durations '"
                 ++ show d ++ "' and '" ++ show d' ++ "'.")
        -- [dur, op]
        Op op : [] ->
          Left ("Infix operator '" ++ show op ++
                "' missing second operand.")
        -- [dur, op, dur, ...]
        Op op : Dur d' : rest' ->
          case op of
            -- [dur, +, dur, ...]
            Add ->
              eval $ (Dur (durAdd d d')) : rest'
            -- [dur, -, dur, ...]
            Subtract ->
              eval $ (Dur (durSubtract d d')) : rest'
            Multiply ->
              eval $ (Dur (durMultiply d d')) : rest'
        -- [dur, op, op]
        Op op : Op op' : _rest' ->
          Left ("Infix operator '"
                 ++ show op ++
                 "' cannot be applied to infix operator '"
                 ++ show op' ++ "'.")
        -- [dur, op, paren, ...]
        Op op : Paren e : rest' ->
          case (eval e) of
            Left err ->
              Left ("While evaluating parenthesized expression '" ++
                    show (Paren e) ++ "':\n" ++
                    err)
            Right result ->
              eval $ (Dur d) : (Op op) : (Dur result) : rest'
        -- [dur, paren, ...]
        Paren e : _rest' ->
          Left ("Operator missing between duration '" ++
                 show d ++ "' and parenthesized expression '" ++
                 show (Paren e) ++ "'.")
    -- [op, ...]
    Op op : _rest ->
      Left ("Infix operator '" ++ show op ++
             "' cannot appear at the beginning of an expression.")
    -- [paren, ...]
    Paren e : rest ->
      case (eval e) of
        Left err ->
          Left ("While evaluating parenthesized expression '" ++
                show (Paren e) ++ "':\n" ++
                err)
        Right result ->
          eval $ (Dur result) : rest
