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
    (Dur d) : [] ->
      Right (normalize d)
    (Paren e) : rest ->
      case (eval e) of
        Left err -> Left err
        Right dur -> eval ((Dur dur) : rest)
    (Dur d) : Op op : (Paren e) : rest ->
      case (eval e) of
        Left err -> Left err
        Right dur -> eval ((Dur d) : (Op op) : (Dur dur) : rest)
    (Dur d) : Op Add : (Dur d') : rest ->
      eval $ (Dur (durAdd d d')) : rest
    (Dur d) : Op Subtract : (Dur d') : rest ->
      eval $ (Dur (durSubtract d d')) : rest
    a : b : c : _rest ->
      Left ("Eval error at \"" ++ show a ++ " " ++ show b ++ " " ++ show c ++ "\"")
    rest ->
      Left ("Eval error at \"" ++ show rest ++ "\"")
