module Parser
  ( parseExpr
  )
where

import Text.Parsec
import Text.Parsec.String

import Expr

num :: Parser Int
num = do
  n <- many1 (oneOf ['0'..'9'])
  return (read n)

duration :: Parser Duration
duration =
  (try durationHMS) <|> ((try durationMS) <|> durationS)

durationHMS :: Parser Duration
durationHMS = do
  hours <- num
  _colon <- char ':'
  mins <- num
  _colon' <- char ':'
  secs <- num
  return (HMS hours mins secs)

durationMS :: Parser Duration
durationMS = do
  mins <- num
  _colon <- char ':'
  secs <- num
  return (MS mins secs)

durationS :: Parser Duration
durationS = do
  secs <- num
  return (S secs)

expr :: Parser Expr
expr = sepEndBy1 term (many (char ' '
                             <|> char '\n'
                             <|> char '\t'))

term :: Parser Term
term =
  (try duration >>= (\d -> return (Dur d)))
  <|> (try operation >>= (\o -> return (Op o)))
  <|> ((char '(' *> expr <* char ')') >>= (\e -> return (Paren e)))

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
