module Expr
  ( Duration (..)
  , Operation (..)
  , Term (..)
  , Expr
  )
where

import Duration

data Operation =
  Add
  | Subtract

instance Show Operation where
  show Add = "+"
  show Subtract = "-"

data Term =
  Dur Duration
  | Op Operation
  | Paren Expr

instance Show Term where
  show (Dur d) = show d
  show (Op op) = show op
  show (Paren e) = show e

type Expr = [Term]
