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
  | Multiply
  deriving (Eq)

instance Show Operation where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"

data Term =
  Dur Duration
  | Op Operation
  | Paren Expr
  deriving (Eq, Show)

type Expr = [Term]
