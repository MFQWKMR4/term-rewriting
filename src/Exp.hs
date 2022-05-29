module Exp where

data Exp =
    Value String     -- non-negative natural numbers
  | Plus Exp Exp  -- e1 + e2
  | Minus Exp Exp -- e1 - e2
  | Times Exp Exp -- e1 + e2

showE1 :: Exp -> String
showE1 (Plus e1 e2)  = showE1 e1 ++ " + " ++ showE2 e2
showE1 (Minus e1 e2) = showE1 e1 ++ " - " ++ showE2 e2
showE1 e             = showE2 e

showE2 :: Exp -> String
showE2 (Times e1 e2) = showE2 e1 ++ " * " ++ showE3 e2
showE2 e             = showE3 e

showE3 :: Exp -> String
showE3 (Value n) =  n
showE3 e         = "(" ++ showE1 e ++ ")"

instance Show Exp where
  show e = showE1 e
