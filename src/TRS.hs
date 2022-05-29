module TRS where

import Data.List

data Term = Var String | Con String | App Term Term
  deriving Eq

data MarkedTerm = MApp MarkedTerm MarkedTerm | MCon String | NF Term

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
isArray (App (App (Con "cons") t) (Con "[]")) = True
isArray (App (App (Con "cons") t) s) = isArray s
isArray _ = False

showArray :: Term -> String
showArray t = "[" ++ showArray' t ++ "]"

showArray' :: Term -> String
showArray' (App (App (Con "cons") t) (App (Con "cons") (Con "[]"))) = show t
showArray' (App (App (Con "cons") t) (Con "[]")) = show t
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

substitute2 :: Term -> Substitution -> MarkedTerm
substitute2 (Var x) sigma
   | Just t <- lookup x sigma = NF t -- 書き換えた結果なのでNF
   | otherwise             = NF (Var x) -- ここはNFと言えるか微妙。ここには来ないはず？？
substitute2 (Con t) _ = NF (Con t) -- 書き換え先が定数、NFと言える
substitute2 (App s t) sigma = MApp (substitute2 s sigma) (substitute2 t sigma) -- 部分項について見ていく。もちろん全体としてはNFと言えない。

rewriteAtRoot2 :: TRS -> Term -> MarkedTerm
rewriteAtRoot2 [] t = NF t -- このtはこれ以上はmatchしないため
rewriteAtRoot2 ((l,r):rls) t
    | Just sigma <- match l t = substitute2 r sigma
    | otherwise = rewriteAtRoot2 rls t

-- marked term を termに変換し、rewriteAtRootを呼び出すことで、nf化したmarked termを得る
rewrite2 :: TRS -> MarkedTerm -> MarkedTerm
rewrite2 rls (NF t) = NF t
rewrite2 rls (MCon t) = rewriteAtRoot2 rls (Con t)
rewrite2 rls (MApp (NF t1) (NF t2)) = rewriteAtRoot2 rls (App t1 t2)
rewrite2 rls (MApp s t) = rewrite2 rls (MApp (rewrite2 rls s) (rewrite2 rls t))

nf2 :: TRS -> MarkedTerm -> Term
nf2 rls (NF t) = t
nf2 rls t = nf2 rls (rewrite2 rls t)

nf3 :: TRS -> Term -> Term
nf3 rls t = nf2 rls t' where t' = rewriteAtRoot2 rls t

--変数は正規系
--定数は書き換えられなければ正規系
--書き換えられた場合は、その結果の正規系を再び調べる
--
--部分項をもつ時、全体で書き換えられるか調べる。
--書き換えられた場合は、その結果の正規系を再び調べる
--
--全体で書き換えられなかった場合は、部分項の正規系を調べる。
--その結果から再構成して、それを書き換える。
--書き換えられた場合は、再度その結果の正規系を再び調べる。
--書き換えられなかった場合はそれが正規系

-- 変更後
--最初に全体がrewriteAtRootに渡される。
-- mainが書き換えられる。markedTermになる。基本はMAppの形となる。
-- あとはそれをnf2に渡せば、再帰的にnfをとる。
-- 定数部分は、termにそのまま直して、NF化する
-- MApp部分は、それぞれの部分項がNFであるならば、作用項の書き換えに入る。
-- それぞれの部分項がNFでない場合は、作用項の書き換えに入る前に、それぞれをNF化する