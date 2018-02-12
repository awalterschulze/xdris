
module Regex

%access public export

data Expr = EmptySet
    | Empty
    | Symbol Char
    | Or Expr Expr
    | Concat Expr Expr
    | Star Expr

Show Expr where
    show EmptySet = "∅"
    show Empty = "ε"
    show (Symbol c) = show c
    show (Or a b) = "(" ++ show a ++ "|" ++ show b ++ ")"
    show (Concat a b) = show a ++ show b
    show (Star a) = "(" ++ show a ++ ")*"

total
nullable : Expr -> Bool
nullable EmptySet = False
nullable Empty = True
nullable (Symbol _) = False
nullable (Or a b) = nullable a || nullable b
nullable (Concat a b) = nullable a && nullable b
nullable (Star _) = True

total
deriv : Expr -> Char -> Expr
deriv EmptySet _ = EmptySet
deriv Empty _ = EmptySet
deriv (Symbol a) c = if a == c then Empty else EmptySet
deriv (Or a b) c = Or (deriv a c) (deriv b c)
deriv (Concat a b) c = if nullable a
    then Or (Concat (deriv a c) b) (deriv b c)
    else Concat (deriv a c) b
deriv (Star a) c = Concat (deriv a c) (Star a)

