module Main where

import System.Environment (getArgs)

import Eval
import Parser

main :: IO Int
main =
  let
    usageMessage = "Usage: hms {expression}"
  in do
    args <- getArgs
    case args of
      [] -> do
        putStrLn usageMessage
        return 1
      _ ->
        let expr = concat args in
          case (parseExpr expr) of
            Left parseErr -> do
              putStr "Parse Error: "
              putStrLn $ show parseErr
              return 1
            Right e ->
              case (eval e) of
                Left evalErr -> do
                  putStrLn "Evaluation Error:"
                  putStrLn evalErr
                  return 1
                Right dur -> do
                  putStrLn $ show dur
                  return 0
