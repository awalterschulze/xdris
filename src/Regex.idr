
module Regex

%access public export

data Expr = EmptySet
    | Empty
    | Letter Char
    | Or Expr Expr
    | Concat Expr Expr
    | Star Expr

Show Expr where
    show EmptySet = "∅"
    show Empty = "ε"
    show (Letter c) = Strings.pack [c]
    show (Or a b) = "(" ++ show a ++ "|" ++ show b ++ ")"
    show (Concat a b) = show a ++ show b
    show (Star a) = "(" ++ show a ++ ")*"

Eq Expr where
    EmptySet == EmptySet = True
    Empty == Empty = True
    (Letter a) == (Letter b) = a == b
    (Or a b) == (Or c d) = a == c && b == d
    (Concat a b) == (Concat c d) = a == c && b == d
    (Star p) == (Star q) = p == q
    _ == _ = False

Var : Type
Var = String

data Pattern = VarsBase Var String Expr
    | VarsGroup Var String Pattern
    | Pair Pattern Pattern
    | Choice Pattern Pattern
    | ZeroOrMore Pattern

Show Pattern where
    show (VarsBase v w e) = v ++ "|" ++ w ++ ":" ++ show e
    show (VarsGroup v w p) = "(" ++ v ++ "|" ++ w ++ ":" ++ show p ++ ")"
    show (Pair a b) = show a ++ show b
    show (Choice a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (ZeroOrMore p) = "(" ++ show p ++ ")*"

Eq Pattern where
    (VarsBase v w e) == (VarsBase v' w' e') = v == v' && w == w' && e == e'
    (VarsGroup v w p) == (VarsGroup v' w' p') = v == v' && w == w' && p == p'
    (Pair a b) == (Pair a' b') = a == a' && b == b'
    (Choice a b) == (Choice a' b') = a == a' && b == b'
    (ZeroOrMore p) == (ZeroOrMore p') = p == p'
    _ == _ = False

total
mkVar : String -> Expr -> Pattern
mkVar name e = VarsBase name "" e

total
newVar : String -> Pattern -> Pattern
newVar name e = VarsGroup name "" e

Env : Type
Env = List (Var, String)

total
mkEnv : Var -> String -> Env
mkEnv v w = [(v, w)]

total
append : Env -> Env -> Env
append a b = a ++ b

total
nullExpr : Expr -> Bool
nullExpr EmptySet = False
nullExpr Empty = True
nullExpr (Letter _) = False
nullExpr (Or a b) = nullExpr a || nullExpr b
nullExpr (Concat a b) = nullExpr a && nullExpr b
nullExpr (Star _) = True

total
nullPat : Pattern -> Bool
nullPat (VarsBase _ _ e) = nullExpr e
nullPat (VarsGroup _ _ p) = nullPat p
nullPat (Pair a b) = nullPat a && nullPat b
nullPat (Choice a b) = nullPat a || nullPat b
nullPat (ZeroOrMore _) = True

total
derivExpr : Expr -> Char -> Expr
derivExpr EmptySet _ = EmptySet
derivExpr Empty _ = EmptySet
derivExpr (Letter a) c = if a == c then Empty else EmptySet
derivExpr (Or a b) c = Or (derivExpr a c) (derivExpr b c)
derivExpr (Concat a b) c = if nullExpr a
    then Or (Concat (derivExpr a c) b) (derivExpr b c)
    else Concat (derivExpr a c) b
derivExpr (Star a) c = Concat (derivExpr a c) (Star a)

total
setEmpty : Pattern -> Pattern
setEmpty (VarsBase v w e) = if nullExpr e
    then (VarsBase v w Empty)
    else (VarsBase v w EmptySet)
setEmpty (VarsGroup v w p) = VarsGroup v w (setEmpty p)
setEmpty (Pair a b) = Pair (setEmpty a) (setEmpty b)
setEmpty (Choice a b) = Choice (setEmpty a) (setEmpty b)
setEmpty (ZeroOrMore p) = ZeroOrMore (setEmpty p)

total
derivPat : Pattern -> Char -> Pattern
derivPat (VarsBase v w e) c = VarsBase v (w ++ Strings.pack [c]) $ derivExpr e c
derivPat (VarsGroup v w p) c = VarsGroup v (w ++ Strings.pack [c]) $ derivPat p c
derivPat (Pair a b) c = if nullPat a
    then Choice (Pair (derivPat a c) b) (Pair (setEmpty a) (derivPat b c))
    else Pair (derivPat a c) b
derivPat (Choice a b) c = Choice (derivPat a c) (derivPat b c)
derivPat (ZeroOrMore p) c = Pair (derivPat p c) (ZeroOrMore p)

total
env : Pattern -> List Env
env (VarsBase v w e) = if nullExpr e
    then [mkEnv v w]
    else []
env (VarsGroup v w p) = [append (mkEnv v w) es | es <- env p]
env (Pair a b) = [append as bs | as <- env a, bs <- env b]
env (Choice a b) = (env a) ++ (env b)
env (ZeroOrMore p) = env p

total
match : Pattern -> String -> List Env
match p s = env $ foldl derivPat p (unpack s)