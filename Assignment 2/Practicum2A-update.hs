{-# LANGUAGE RankNTypes #-}
module Practicum2A where

{-
Name:           <Name and family name>
VU-net id:      <VU-net id>
Student number: <Student number>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <in case you used sources such as books or webpages
                 please mention them here>
-}

----------------------------
-- Exercise Tower of Hanoi
----------------------------

type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi = undefined


-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals = undefined

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = undefined

-- Exercise 3
threefolds :: [Integer]
threefolds = undefined

-- Exercise 4
removeif :: (a -> Bool) -> [a] -> [a]
removeif = undefined

nothreefolds :: [Integer]
nothreefolds = undefined

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds = undefined

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds = undefined

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds = undefined

-- Exercise 8
eratosthenes :: [Integer]
eratosthenes = undefined

-- Exercise 9
fibonacci :: [Integer]
fibonacci = undefined

-- -----------------------
-- Exercise Church Numerals
-- -----------------------
-- we need polymorphic types for the Church Numerals 
type ChurchNumeral = forall a . (a -> a) -> a -> a

-- Exercise 1
churchnumeral :: (Eq a, Num a) => a -> ChurchNumeral 
churchnumeral n =
  if   n == 0
  then \s z -> z
  else \s z -> churchnumeral (n - 1) s (s z)

backtointeger :: (Num a) => ChurchNumeral -> a
backtointeger cn = cn (+1) 0



{- Show usage here -}

-- Exercise 2
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality x y = undefined

-- Exercise 3 given as example
successor ::  ChurchNumeral -> ChurchNumeral
successor x s z  = s ( x s z ) 
 
-- Exercise 4
successorb :: ChurchNumeral -> ChurchNumeral
successorb x s z = undefined

-- Exercise 5
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  backtointeger ( f ( churchnumeral n ) ) 

-- Exercise 6
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition x y s z  = undefined

multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication x y s  = undefined

exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation x y  = undefined 

-- Exercise 7
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2 f m n  = undefined


-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes = undefined

-- Exercise 2
height :: BinaryTree a -> Integer
height = undefined

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes = undefined

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror = undefined

-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten = undefined

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap = undefined

-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan = undefined

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan = undefined

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree = undefined

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement = undefined

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert = undefined

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree = undefined

-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove = undefined
