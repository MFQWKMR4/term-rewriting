module Parser (readTRSFile) where

import Data.List
import Data.Char
import Text.ParserCombinators.Parsec
import TRS
import Exp

-- Checking the variable condition.

variables :: Term -> [String]
variables (Con _)   = []
variables (Var x)   = [x]
variables (App s t) = nub (variables s ++ variables t)

extraVariables :: Rule -> [String]
extraVariables (l, r) = nub [ x | x <- variables r, not (elem x (variables l)) ]


-- Tokenizers

alphaNumBar :: Parser Char
alphaNumBar = try alphaNum <|> char '_'

integer :: Parser Int
integer = do
  spaces
  x <- many1 digit
  spaces
  return (read x)

lowerWord :: Parser String
lowerWord = do
  spaces
  c <- lower
  s <- many alphaNumBar
  spaces
  return (c : s)

upperWord :: Parser String
upperWord = do
  spaces
  c <- upper
  s <- many alphaNumBar
  spaces
  return (c : s)

keyword :: String -> Parser ()
keyword s = do
  spaces
  _ <- string s
  spaces
  return ()

translateConstant :: String -> String
translateConstant "nil" = "[]"
translateConstant s = s

-- Parsers

e1f :: Term -> Term -> Term
e1f t1 t2 = App (Con "+") (App t1 t2)

e3Arrayf :: Term -> Term -> Term
e3Arrayf t1 t2 = App (Con "cons") (App t1 t2)

foldArrayF :: Term -> Term -> Term
foldArrayF t acc = App (App cons t) acc

parseE1 :: Parser Term
parseE1 = do
  es <- sepBy1 parseE2 (keyword "+")
  return (foldl1 e1f es)

parseE2 :: Parser Term
parseE2 = do
  es <- many1 parseE3
  return (foldl1 App es)

parseE3 :: Parser Term
parseE3 =
  try parseArray2 <|>
  try parseSimpleTerm <|>
  parseE3a

parseE3a :: Parser Term
parseE3a = do
  keyword "("
  e <- parseE1
  keyword ")"
  return e
  
parseF1 :: Parser Term
parseF1 = do
  es <- sepBy1 parseF2 (keyword "+")
  return (foldl1 e1f es)

parseF2 :: Parser Term
parseF2 = do
  es <- many1 parseF3
  return (foldl1 App es)

parseF3 :: Parser Term
parseF3 =
  try parseSimpleTerm <|>
  parseF3a

parseF3a :: Parser Term
parseF3a = do
  keyword "("
  e <- parseF1
  keyword ")"
  return e

parseParen :: Parser a -> Parser a
parseParen p = do
  keyword "("
  x <- p
  keyword ")"
  return x

parseArray :: Parser a -> Parser a
parseArray p = do
  keyword "["
  x <- p
  keyword "]"
  return x

parseNumber :: Parser Term
parseNumber = do
  n <- integer
  return (num n)

parseVariable :: Parser Term
parseVariable = do
  x <- upperWord
  return (Var x)

parseConstant :: Parser Term
parseConstant = do
  c <- lowerWord
  return (Con (translateConstant c))

parseSimpleTerm :: Parser Term
parseSimpleTerm =
  try parseNumber <|>
  try parseVariable <|>
  try parseConstant <|>
  try (parseParen parseTerm) <|>
  try (parseArray parseTerm2)

parseTerm :: Parser Term
parseTerm = do
  ts <- sepBy1 parseSimpleTerm spaces
  return (foldl1 App ts)

parseTerm2 :: Parser Term
parseTerm2 = do
  ts <- sepBy parseSimpleTerm (keyword ",")
  return (foldr foldArrayF (App cons el) ts)

parseArray2 :: Parser Term
parseArray2 = do
  ts <- sepBy1 parseF1 (keyword ":")
  return (foldr1 foldArrayF ts)

parseRule :: Parser Rule
parseRule = do
  l <- parseE1
  keyword "->"
  r <- parseE1
  keyword ";"
  case extraVariables (l, r) of
    [] -> return (l, r)
    x : _ -> fail ("Unknown variable: " ++ x)

parseTRSFile :: Parser TRS
parseTRSFile = many parseRule

readTRSFile :: String -> IO (Either ParseError TRS)
readTRSFile path = parseFromFile parseTRSFile path
