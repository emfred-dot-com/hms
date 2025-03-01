module Expr
  ( Duration (..)
  , Operation (..)
  , Term (..)
  , Expr
  )
where

import Data.List

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
  deriving (Eq)

instance Show Term where
  show (Dur d) = show d
  show (Op op) = show op
  show (Paren e) =
    "(" ++ (concat $ intersperse " " $ map show e) ++ ")"

type Expr = [Term]
