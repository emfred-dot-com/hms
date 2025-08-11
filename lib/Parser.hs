module Parser
  ( parseExpr
  )
where

import Text.Parsec
import Text.Parsec.String

import Data.Ratio
import Data.Scientific

import Expr

sci :: Parser Duration
sci = (try frac) <|> ((try decimal) <|> int)

frac :: Parser Duration
frac = do
  num <- digits
  _slash <- char '/'
  denom <- digits
  let d :: Double
      d = fromRational (read num % read denom)
  return $ Duration (fromFloatDigits d)

decimal :: Parser Duration
decimal = do
  whole <- digits
  _dot <- char '.'
  fractional <- digits
  return $ Duration $
    read $ whole ++ "." ++ fractional

int :: Parser Duration
int = do
  num <- digits
  return $ Duration $
    read num

digits :: Parser String
digits = many1 $ oneOf ['0'..'9']

duration :: Parser Duration
duration = (try durationHMS) <|> ((try durationMS) <|> durationS)

durationHMS :: Parser Duration
durationHMS = do
  hours <- sci
  _colon <- char ':'
  mins <- sci
  _colon' <- char ':'
  secs <- sci
  return $
    (hours * 60.0 * 60.0) + (mins * 60.0) + secs

durationMS :: Parser Duration
durationMS = do
  mins <- sci
  _colon <- char ':'
  secs <- sci
  return $
    (mins * 60.0) + secs

durationS :: Parser Duration
durationS = do
  secs <- sci
  return secs

expr :: Parser Expr
expr = many1 term

term :: Parser Term
term =
  spaces *>
    ((try duration >>= (\d -> return (Dur d)))
    <|> (try operation >>= (\o -> return (Op o)))
    <|> ((char '(' *> expr <* char ')') >>= (\e -> return (Paren e))))
    <* spaces

operation :: Parser Operation
operation =
  (char '+' >> return Add)
  <|> (char '-' >> return Subtract)
  <|> (char '*' >> return Multiply)

exprTopLevel :: Parser Expr
exprTopLevel = do
  e <- expr
  eof
  return e

parseExpr :: String -> Either ParseError Expr
parseExpr = parse exprTopLevel ""
