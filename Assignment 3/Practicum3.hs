module Practicum3 where

{-
Name:           <Andrea Di Dio>
VU-net id:      <ado380>
Student number: <2593888>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        http://learnyouahaskell.com/making-our-own-types-and-typeclasses
                https://wiki.haskell.org/Combinatory_logic
                https://wiki.haskell.org/Combinator
                http://brandon.si/code/designing-a-module-for-combinatory-logic-in-haskell/
                http://lambda-the-ultimate.org/node/3719
                http://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Show.html
                https://www.haskell.org/tutorial/stdclasses.html
-}

-- Exercises Arithmetical Expressions
data IntExp  = Lit Int | Add IntExp IntExp | Mul IntExp IntExp
  deriving Show

showintexp :: IntExp -> String
showintexp x = 
  case x of
    (Lit n) -> show n
    (Add (Lit l) (Lit n)) -> "(" ++ showintexp (Lit l) ++ "+" ++ showintexp (Lit n) ++ ")"
    (Mul (Lit l) (Lit n)) -> "(" ++ showintexp (Lit l) ++ "*" ++ showintexp (Lit n) ++ ")"
-- showintexp (Lit 5) == "5"
-- showintexp ( Add (Lit 5) (Lit 7) ) == "(5+7)"
-- showintexp ( Mul (Lit 5) (Lit 7) ) == "(5*7)"

evalintexp :: IntExp -> Int
evalintexp x = 
  case x of
    (Lit n) -> n
    (Add (Lit l) (Lit n)) -> ( evalintexp (Lit l) + evalintexp (Lit n) )
    (Mul (Lit l) (Lit n)) -> ( evalintexp (Lit l) * evalintexp (Lit n) )
-- evalintexp (Lit 5) == 5
-- evalintexp (Add (Lit 7) (Lit 5)) == 12
-- evalintexp (Mul (Lit 7) (Lit 5)) == 35

-- Exercises Combinatory Logic
data Term = S | K | I | App Term Term

instance Show Term where
  show a = showterm a

showterm :: Term -> String
showterm x = 
  case x of 
    S -> "S"
    K -> "K"
    I -> "I"
    (App l n) -> "(" ++ showterm l ++ showterm n ++ ")"
-- showterm (App (App (App S K) I) (App I I)) == "(((SK)I)(II))"
-- showterm S == "S"

isredex :: Term -> Bool
isredex x = 
  case x of
    (App I l) -> True
    (App (App K l) n) -> True
    (App (App (App S l) n) y) -> True
    otherwise -> False
-- isredex (App (App) (App S I) K) S) == True
-- isredex (App (App I K) (App (App K I) K)) == False
-- isredex (App I S) == True

isnormalform :: Term -> Bool
isnormalform x =
  case x of
    I -> True
    K -> True
    S -> True
    (App K l) -> isnormalform l
    (App S l) -> isnormalform l
    (App (App S l) n) | isnormalform (App S l) && (isnormalform n) -> True
                      | otherwise -> False
    otherwise -> False
-- isnormalform I == True
-- isnormalform (App K I) == True
-- isnormalform (App K (App I I)) == False

headstep :: Term -> Term
headstep x = 
  case x of
    _ | isredex x == False -> x
    (App I l) -> l
    (App (App K l) n) -> l
    (App (App (App S l) n) y) -> (App (App l y) (App n y))
-- headstep (App (App (App S I) K) S) == ((IS)(KS))
-- headstep (App (App I K) (App (App K I) K)) == ((IK)((KI)K))
-- headstep (App I K) == K

-- Exercises Equational Specifications
data Thing = C | D | X
  deriving (Show, Eq, Bounded, Enum)

nxt :: Thing -> Thing
nxt n = 
  case n of
    C -> X
    X -> D
    D -> C
-- nxt C == X
-- nxt (nxt C) == D
-- nxt (nxt (next X)) == X

-- 
data I = T | F --I treat 1 as T and 0 as F (True/False)
  deriving (Show, Eq, Bounded, Enum)

s :: I -> I
s x =
  case x of
    F -> T -- S(0) = 1
    T -> F -- S(1) = 0

p :: I -> I
p x = 
  case x of
    T -> F -- P(1) = 0
    F -> T -- P(0) = 1

-- p (s T) == T
-- s (p T) == T
-- p (s F) == F
-- s (p F) == F
-- s F == T
-- p F == T
