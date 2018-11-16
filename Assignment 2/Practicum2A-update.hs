{-# LANGUAGE RankNTypes #-}
module Practicum2A where

{-
Name:           <Andrea Di Dio>
VU-net id:      <ado380>
Student number: <2593888>
Discussed with: <>
Remarks:        <>
Sources:        https://wiki.haskell.org/99_questions/Solutions/39
                http://learnyouahaskell.com/zippers
                https://dkalemis.wordpress.com/2014/01/23/trees-in-haskell/
                https://stackoverflow.com/questions/21202010/implementing-binary-search-tree-insertion-in-haskell
                https://stackoverflow.com/questions/6462749/church-numerals-in-haskell
                https://www.mjoldfield.com/atelier/2011/01/church-numerals.html
                https://karczmarczuk.users.greyc.fr/Essays/church.html
                http://learnyouahaskell.com/syntax-in-functions
                https://wiki.haskell.org/Let_vs._Where
                http://zvon.org/other/haskell/Outputsyntax/letQexpressions_reference.html
                https://stackoverflow.com/questions/40836222/haskell-guard-inside-case-statement
                -}

----------------------------
-- Exercise Tower of Hanoi
----------------------------

type Rod = String
type Move = (Integer, Rod, Rod)
hanoi :: Integer -> Rod -> Rod -> Rod -> [Move]
hanoi n source auxiliary destination = 
  case n of
    0 -> []
    1 -> [(1, source, destination)]
    n -> hanoi (n - 1) source destination auxiliary ++
         [(n, source, destination)] ++
         hanoi (n - 1) auxiliary source destination 

-- hanoi 0 "A" "B" "C" == []
-- hanoi 1 "A" "B" "C" == [(1,"A","C")]
-- hanoi 3 "A" "B" "C" == [(1,"A","C"),(2,"A","B"),(1,"C","B"),(3,"A","C"),(1,"B","A"),(2,"B","C"),(1,"A","C")]
{-
hanoi 4 "A" "B" "C" == [(1,"A","B"),(2,"A","C"),(1,"B","C"),(3,"A","B"),(1,"C","A"),
                        (2,"C","B"),(1,"A","B"),(4,"A","C"),(1,"B","C"),(2,"B","A"),(1,"C","A"),(3,"B","C"),
                        (1,"A","B"),(2,"A","C"),(1,"B","C")]
-}


-- -------------------------
-- Exercises Infinite Lists
-- -------------------------

-- Exercise 1
naturals :: [Integer]
naturals = 
  let head = 1
    in head : map(\x -> x + 1) naturals
-- naturals == [1,2,3,4,5,6,7,8,9,...]

-- Exercise 2
zeroesandones :: [Integer]
zeroesandones = 
  let head = 0
    in head : 1 : zeroesandones
-- zerosandones == [0,1,0,1,0,1,...]

-- Exercise 3
threefolds :: [Integer]
threefolds = 
  let input = naturals
    in 0 : map(\x -> x * 3) input
-- threefolds == [0,3,6,9,12,15,18,...]

-- Exercise 4
removeif :: (a -> Bool) -> [a] -> [a]
removeif pred n =
  case n of
    [] -> []
    (h:t) -> filter(not.pred) n

nothreefolds :: [Integer]
nothreefolds = 
  let pred = (\x -> mod x 3 == 0)
    in removeif pred naturals
-- nothreefolds == [1,2,4,5,7,8,10,11,13,...]

-- Exercise 5
allnfolds :: Integer -> [Integer]
allnfolds n = 
  let input = naturals
    in 0 : map(\x -> x * n) input
-- allnfolds 2 == [0,2,4,6,8,10,...]
-- allnfolds 3 == threefolds

-- Exercise 6
allnaturalsexceptnfolds :: Integer -> [Integer]
allnaturalsexceptnfolds n =
  let pred = (\x -> mod x n == 0)
    in  removeif pred naturals
--allnaturalsexceptnfolds 3 == nothreefolds
--allnaturalsexceptnfolds 2 == [1,3,5,7,9,11,13,15,...]

-- Exercise 7
allelementsexceptnfolds :: Integer -> [Integer] -> [Integer]
allelementsexceptnfolds n l = 
  case l of 
    [] -> []
    (h:t) -> let pred = (\x -> mod x n == 0)
                in removeif pred l
-- allelementsexceptnfolds 2 [1,2,3,4,5,6,7,8,9,10] == [1,3,5,7,9]
-- allelementsexceptnfolds 3 [3,6,9,12,15,18] == []

-- Exercise 8
eratosthenes :: [Integer]
eratosthenes = initList [2..]
    where initList (p:t) = p : initList [n | n <- t, mod n p /= 0]
-- eratosthenes == [2,3,5,7,1,13,17,19,23,29,...]

-- Exercise 9
fibonacci :: [Integer]
fibonacci = 
  let remaining = tail(fibonacci)
    in 0 : 1 : zipWith (+) fibonacci remaining
--fibonacci == [0,1,1,2,3,5,8,13,21,34,55,89,144,...]

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



{- backtointeger (churchnumeral 4) == 4
   backtointeger (churchnumeral 10) == 10
 -}

-- Exercise 2
churchequality ::  ChurchNumeral  -> ChurchNumeral  -> Bool
churchequality x y = if backtointeger (x) == backtointeger (y) then True else False

-- Exercise 3 given as example
successor ::  ChurchNumeral -> ChurchNumeral
successor x s z  = s ( x s z ) 
 
-- Exercise 4
successorb :: ChurchNumeral -> ChurchNumeral
successorb x s z = x s (s z)

-- Exercise 5
apply1 :: (Eq a, Num a) => (ChurchNumeral-> ChurchNumeral) ->  a -> a
apply1 f n =  backtointeger ( f ( churchnumeral n ) ) 

-- Exercise 6
addition :: ChurchNumeral -> ChurchNumeral -> ChurchNumeral
addition x y s z  = y s (x s z)
-- backtointeger (addition (churchnumeral 3) (churchnumeral 5)) == 8
-- backtointeger (addition (churchnumeral 5) (churchnumeral 5)) == 10


multiplication ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral
multiplication x y s  = y (x s)
-- backtointeger (multiplication (churchnumeral 3) (churchnumeral 5)) == 15
-- backtointeger (multiplication (churchnumeral 5) (churchnumeral 5)) == 25

exponentiation ::  ChurchNumeral -> ChurchNumeral -> ChurchNumeral 
exponentiation x y  = y (x)
-- backtointeger (exponentiation (churchnumeral 2) (churchnumeral 3)) == 8
-- backtointeger (exponentiation (churchnumeral 2) (churchnumeral 5)) == 32

-- Exercise 7
apply2 :: (Eq a, Num a) => (ChurchNumeral -> ChurchNumeral -> ChurchNumeral) -> a -> a -> a
apply2 f m n  = backtointeger (f (churchnumeral m) (churchnumeral n))
-- apply2 addition 3 5 == 8
-- apply2 multiplication 3 5 == 15
-- apply2 exponentiation 3 5 == 243

-- ---------------------
-- Exercises Binary Trees
-- ---------------------
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

--Useful Function in Description:
single :: a -> BinaryTree a
single x = Node (Leaf) x (Leaf)

-- Exercise 1
numberofnodes :: BinaryTree a -> Integer
numberofnodes n =
  case n of
    Leaf -> 0
    (Node x y z) -> (numberofnodes x) + (numberofnodes z) + 1
-- numberofnodes (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == 3
-- numberofnodes (Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) 3 (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 Leaf))) == 7

-- Exercise 2
height :: BinaryTree a -> Integer
height n = 
  case n of 
    Leaf -> 0
    (Node x y z)  | height x > height z -> height x + 1
                  | height x < height z -> height z + 1
                  | otherwise -> height x + 1
-- height (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == 2
-- height (Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) 3 (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 Leaf))) == 3

-- Exercise 3
sumnodes :: (Num a) => BinaryTree a -> a
sumnodes n = 
  case n of
    Leaf -> 0
    (Node x y z) -> (sumnodes x) + (sumnodes z) + y
-- sumnodes (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == 3
-- sumnodes (Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) 3 (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 Leaf))) == 21

-- Exercise 4
mirror :: BinaryTree a -> BinaryTree a
mirror n = 
  case n of
    Leaf -> Leaf
    (Node x y z) -> (Node (mirror z) y (mirror x))
-- mirror (Node (single 5) 3 Leaf) == Node Leaf 3 (Node Leaf 5 Leaf)
-- mirror (Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) 3 (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 Leaf)))
--        == Node (Node (Node Leaf 6 Leaf) 5 (Node Leaf 4 Leaf)) 3 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 0 Leaf))


-- Exercise 5
flatten :: BinaryTree a -> [a]
flatten n = 
  case n of
    Leaf -> []
    (Node x y z) -> (flatten x) ++ [y] ++ (flatten z)
-- flatten (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == [0,1,2]
-- flatten (Node (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) 3 (Node (Node Leaf 4 Leaf) 5 (Node Leaf 6 Leaf))) == [1,2,3,4,5,6]

-- Exercise 6
treemap :: (a -> b) -> BinaryTree a -> BinaryTree b
treemap func n = 
  case n of 
    Leaf -> Leaf
    (Node x y z) -> (Node (treemap func x) (func y) (treemap func z))
-- treemap (+1) (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
-- treemap (\l -> l * 2) (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == Node (Node Leaf 0 Leaf) 2 (Node Leaf 4 Leaf)


-- -------------------------
-- Exercises Binary Search Trees
-- -------------------------

-- Exercise 1
smallerthan :: (Ord a) => a -> BinaryTree a -> Bool
smallerthan l n =
  case n of
    Leaf -> True
    (Node x y z) | l < y -> False
                 | l > y -> smallerthan l z
                 | otherwise -> False
-- smallerthan 4 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == True
-- smallerthan 0 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == False

largerthan :: (Ord a) => a -> BinaryTree a -> Bool
largerthan l n = 
  case n of
    Leaf -> True
    (Node x y z) | l > y -> False
                 | l < y -> largerthan l x
                 | otherwise -> False
-- largerthan largerthan 0 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == False
-- largerthan 0 (Node (Node Leaf 4 Leaf) 1 (Node Leaf 2 Leaf)) == True

-- Exercise 2
isbinarysearchtree :: (Ord a) => BinaryTree a -> Bool
isbinarysearchtree  n = 
  case n of 
    Leaf -> True
    (Node x y z) | (smallerthan y x) && (largerthan y z)
                   && (isbinarysearchtree z) 
                   && (isbinarysearchtree x) -> True
                 | otherwise -> False
-- isbinarysearchtree (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == True
-- isbinarysearchtree (Node (Node Leaf 0 Leaf) 3 (Node Leaf 2 Leaf)) == False

-- Exercise 3
iselement :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
iselement l n = 
  case n of 
    Leaf -> False
    (Node x y z) | l == y -> True
                 | otherwise -> ((iselement l z) || (iselement l x))
-- iselement 4 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == False
-- iselement 2 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == True

-- Exercise 4
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert l n = 
  case n of
    Leaf -> (single l)
    (Node x y z) | iselement l n -> (Node x y z)
                 | l < y -> (Node (insert l x) y z)
                 | otherwise -> (Node x y (insert l z))
-- insert 4 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf))
--            == Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 (Node Leaf 4 Leaf))
-- insert 0 (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
--            == Node (Node (Node Leaf 0 Leaf) 1 Leaf) 2 (Node Leaf 3 Leaf)
-- insert 0 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf))
--            == Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)

-- Exercise 5
createbinarysearchtree :: (Ord a, Eq a) => [a] -> BinaryTree a
createbinarysearchtree n =
  case n of 
    (h:t) | length n == 1 -> (Node Leaf h Leaf)
          | otherwise -> (insert h (createbinarysearchtree t))
-- createbinarysearchtree [1] == Node Leaf 1 Leaf
-- createbinarysearchtree [1,2,3,4,0] == Node Leaf 0 (Node (Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 Leaf) 4 Leaf)
-- createbinarysearchtree [1,2,3,4,0,2,3] == Node Leaf 0 (Node (Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 Leaf) 4 Leaf)

-- Exercise 6
remove :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
remove l n =
  case n of
    Leaf -> Leaf
    (Node x y z) | iselement l n == False -> n
                 | otherwise -> 
                      let toList = flatten n
                        in createbinarysearchtree (filter (\u -> u /= l) (toList))
-- remove 0 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == Node (Node Leaf 1 Leaf) 2 Leaf
-- remove 4 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)
-- remove 1 (Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)) == Node (Node Leaf 0 Leaf) 2 Leaf