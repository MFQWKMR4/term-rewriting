module TRS where

import Data.List

data Term = Var String | Con String | App Term Term
  deriving Eq

type Rule = (Term, Term)

type TRS = [Rule]

type Substitution = [(String, Term)]

-- Peano numbers
zero :: Term
zero = Con "0"

suc :: Term
suc = Con "s"

num :: Int -> Term
num 0 = zero
num n = App suc (num (n-1))

el :: Term
el = Con "[]"

cons :: Term
cons = Con "cons"

-- Pretty printers for terms.

isArray :: Term -> Bool
isArray (App (App (Con "cons") t) (App (Con "cons") (Con "[]"))) = True
isArray (App (Con t) (Con "[]")) = True
isArray (App (App (Con "cons") t) s) = isArray s
isArray _ = False

showArray :: Term -> String
showArray t = "[" ++ showArray' t ++ "]"

showArray' :: Term -> String
showArray' (App (App (Con "cons") t) (App (Con "cons") (Con "[]"))) = show t
showArray' (App (App (Con "cons") t) s) = show t ++ "," ++ showArray' s
showArray' _ = error "unsupported term"

isPeanoNumber :: Term -> Bool
isPeanoNumber (Con "0") = True
isPeanoNumber (App (Con "s") t) = isPeanoNumber t
isPeanoNumber _ = False

peanoNumberToInt :: Term -> Int
peanoNumberToInt (Con "0") = 0
peanoNumberToInt (App (Con "s") t) = 1 + peanoNumberToInt t
peanoNumberToInt _ = error "unsupported term"

showTerm :: Term -> String
showTerm t
  | isPeanoNumber t = show (peanoNumberToInt t)
  | isArray t = showArray t
  | otherwise = case t of
    App u r -> showTerm u ++ " " ++ showSimpleTerm r
    _ -> showSimpleTerm t

showSimpleTerm :: Term -> String
showSimpleTerm t
  | isPeanoNumber t = show (peanoNumberToInt t)
  | isArray t = showArray t
  | otherwise = case t of
    Var x -> x
    Con f -> f
    _ -> "(" ++ showTerm t ++ ")"

instance Show Term where
  show = showTerm

showRule :: Rule -> String
showRule (s, t) = show s ++ " -> " ++ show t ++ " ;"

showTRS :: TRS -> String
showTRS rules =
  unlines [ showRule rule | rule <- rules ]

-- implementation
substitute :: Term -> Substitution -> Term
substitute (Var x) sigma
   | Just t <- lookup x sigma = t
   | otherwise             = Var x
substitute t@(Con _) _ = t
substitute (App s t) sigma = App (substitute s sigma) (substitute t sigma)

match' :: Substitution -> [(Term, Term)] -> Maybe Substitution
match' sigma [] = Just sigma
match' sigma (((Var x), u):pairs)
    | Just t <- lookup x sigma = if t == u then match' sigma pairs else Nothing
    | otherwise = match' ((x, u):sigma) pairs
match' sigma (((Con s), (Con t)):pairs) = if s == t then match' sigma pairs else Nothing
match' _ (((Con _), _):_) = Nothing
match' sigma (((App s t), (App u v)):pairs) = match' sigma ((s, u):(t, v):pairs)
match' _ _ = Nothing

match :: Term -> Term -> Maybe Substitution
match s t = match' [] [(s,t)]

rewrite :: TRS -> Term -> Maybe Term
rewrite [] _ = Nothing
rewrite ((l,r):rls) t
    | Just sigma <- match l t = Just (substitute r sigma)
    | otherwise = rewrite rls t

nf :: TRS -> Term -> Term
nf _ t@(Var _) = t
nf rls s@(Con _)
    | Just t <- rewrite rls s = nf rls t
    | otherwise = s
nf rls (App s t)
    | Just u <- rewrite rls (App s t) = nf rls u
    | otherwise =
        case rewrite rls a' of
        Just u -> nf rls u
        Nothing -> a'
        where a' = App (nf rls s) (nf rls t)
