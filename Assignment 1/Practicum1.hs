



module Practicum1 where

{-
Name:           <Andrea Di Dio>
VU-net id:      <ado380>
Student number: <2593888>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        http://learnyouahaskell.com/chapters
		https://www.haskell.org/hoogle/
		https://www.youtube.com/watch?v=02_H3LjqMr8
		https://wiki.haskell.org/Tutorials
		http://learn.hfm.io/
		http://users.umiacs.umd.edu/~hal/docs/daume02yaht.pdf
-}

-- Below you will find templates for the exercises. For each exercise,
-- replace 'undefined' by your definition and supply at least two different
-- meaningful tests to show that your code produces sane results. The
-- tests may be given in comments (see exercise 1).

-- Exercise 1
maxi :: Integer -> Integer -> Integer
maxi x y = if y >= x then y else x

-- maxi 2 3 == 3
-- maxi 3 2 == 2

-- Exercise 2
fourAscending :: Integer -> Integer -> Integer -> Integer -> Bool
fourAscending w x y z = if w < x && x < y && y < z then True else False

-- fourAscending 1 2 3 4 == True
-- fourAscending 1 2 4 3 == False
-- fourAscending 1 1 1 1 == False

-- Exercise 3
fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual w x y z = if w == x && x == y && y == z then True else False 

-- fourEqual 1 2 3 4 == False
-- fourEqual 1 1 1 1 == True
-- fourEqual 1 1 2 2 == False
-- fourEqual 1 2 2 2 == False

-- Exercise 4
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent w x y z = if (w /= x && w /= y && w /= z) && (x /= y && x /= z) && y /= z then True else False  

--fourDifferent 1 1 2 1 == False
--fourDifferent 1 2 3 4 == True
--fourDifferent 1 2 3 1 == False

-- Exercise 5
{-
   threeDifferent :: Integer -> Integer -> Integer -> Bool
   threeDifferent a b c = ( ( a /= b) && (b /= c) )
   fails when first and last are the same, should return false but returns true,
   for example: threeDifferent 3 4 3 returns True.
-}

-- Exercise 6
factorial :: Integer -> Integer
factorial n | (n == 0) = 1
            | (n > 0) = n * factorial(n - 1)

-- factorial 3 == 6
-- factorial 4 == 24 

-- Exercise 7
fib :: Integer -> Integer
fib n | (n == 0) = 0
      | (n == 1) = 1
      | (n >= 2) = fib(n - 1) + fib(n - 2)

-- fib 10 == 55
-- fib 15 == 610
-- fib 20 == 6765
-- fib 30 == 832040

-- Exercise 8
-- it is possible to define auxiliary functions
strangeSummation :: Integer -> Integer
makeList :: Integer -> [Integer]
makeList i = [i..i+7]
strangeSummation  n = sumList(makeList n)

--strangeSummation 10 == 108
--strangeSummation 5 == 68

-- Exercise 9
lengthList :: [Integer] -> Integer
lengthList []  = 0
lengthList (h:t) = 1 + lengthList t

-- lengthList [1,2,3] == 3
-- lengthList [1,2,3,4,5] == 5

lengthListAlternative :: [Integer] -> Integer
lengthListAlternative l =
  case l of
    [] -> 0
    (h:t) -> 1 + (lengthListAlternative t)

sumList :: [Integer] -> Integer
sumList l =
  case l of
    [] -> 0
    (h:t) -> h + sumList t

-- sumList [1,2,3] == 6
-- sumList [1,2,25] == 28
-- sumList [1,1,1,1,1,1,1,1,1,1,1,1,1] == 13

-- Exercise 10
doubleList :: [Integer] -> [Integer]
doubleList [] = []
doubleList (h:t) = (h * 2) : doubleList t

--doubleList [1,2,3] == [2,4,6]
--doubleList [4,10,20] == [8,20,40]

-- Exercise 11
myappend :: [a] -> [a] -> [a]
myappend l x = 
  case l of
    [] -> x
    (h:t) -> h : myappend t x

--myappend [1,2] [3,4,5,6,7] == [1,2,3,4,5,6,7]
--myappend [] [3,4,5,6,7] == [3,4,5,6,7]
--myappend [1,2] [] == [1,2]

-- Exercise 12
myreverse :: [a] -> [a]
myreverse l = 
  case l of
    [] -> []
    (h:t) -> myappend (myreverse t) [h]

--myreverse [1,2] == [2,1]
--myreverse [1,2,3,4,5] == [5,4,3,2,1]

-- Exercise 13
mymember :: (Eq a) => a -> [a] -> Bool
mymember l x = 
  case x of
    [] -> False
    (h:t) -> if (l == h || mymember l t) then True else False

--mymember 0 [] == False
--mymember 0 [1,2,0] == True
--mymember 100 [1,2,3,4,100,5,6] == True
--mymember 100 [1,2,3,4,5,6] == False

-- Exercise 14
mysquaresum :: [Integer] -> Integer
mysquaresum l =
  case l of
    [] -> 0
    (h:t) -> h * h + mysquaresum t

--mysquaresum [1,2,3,4,5,6] == 91
--mysquaresum [] == 0
--mysquaresum [2,2,2,2,2,2,2,2] == 32

-- Exercise 15
range :: Integer -> Integer -> [Integer]
range l x | (l > x) = []
          | (l == x) = l:[]
          | otherwise = l : range (l+1) x

--range 0 0 == [0]
--range 2 1 == []
--range 1 10 == [1,2,3,4,5,6,7,8,9,10]
--range 0 3 == [0,1,2,3]

-- Exercise 16
myconcat :: [[a]] -> [a]
myconcat l = 
  case l of
    [] -> []
    (h:t) -> myappend h (myconcat t)

--myconcat [[1,2,3], [4]] == [1,2,3,4]
--myconcat [[1,2,3], [4,5,6], [7,8,9]] == [1,2,3,4,5,6,7,8,9]
--myconcat [[1,2,3], [4,5,6], [7,8,9], []] == [1,2,3,4,5,6,7,8,9]
--myconcat [] == []


-- Exercise 17
insert :: Ord a => a -> [a] -> [a]
insert x l =
  case l of
    [] -> x:[]
    (h:t) -> if x < h then x:h:t else h : insert x t

--insert 0 [] == [0]
--insert 7 [1,2,3,4,5,6,8,9] == [1,2,3,4,5,6,7,8,9]
--insert 7 [1,2,3,4,5,6,7,8,9] == [1,2,3,4,5,6,7,7,8,9]
--insert 10 [1,2,3,4,5,6,8,9] == [1,2,3,4,5,6,7,8,9,10]

insertionsort :: Ord a => [a] -> [a]
insertionsort l =
  case l of
    [] -> []
    [x] -> [x]
    (h:t) -> insert h (insertionsort t)

--insertionsort [8,6,3,6,7,3,7,8,9,1,10] == [1,3,3,6,6,7,7,8,8,9,10]
--insertionsort [1] == [1]
--insertionsort [] == []
--insertionsort [5,1,3,9,2,8,4,3,10,100,6] == [1,2,3,3,4,5,6,8,9,10,100]

-- Exercise 18

--filter examples:
filterOdd :: [Integer] -> [Integer]
filterOdd x = filter odd x

--filterOdd [1,2,3,4,5,6,7] == [1,3,5,7]
--filterOdd [0] == []

filterLowercase :: String -> String
filterLowercase n = filter (`elem` ['a'.. 'z']) n

--filterLowercase "Hello" == "ello"
--filterLowercase "HELLO" == ""

filterPosAnon :: [Integer] -> [Integer]
filterPosAnon n = filter (\x -> x >= 0) n

--filterPosAnon [-2,0,3,-10] == [0,3]
--filterPosAnon [-1] == []

filterSmaller :: Ord a => [a] -> a -> [a]
filterSmaller l x =
  case l of
    [] -> []
    (h:t) -> filter (\x -> x <= h) t

filterLarger :: Ord a => [a] -> a -> [a]
filterLarger l x =
  case l of
    [] -> []
    (h:t) -> filter (\x -> x > h) t 

quicksort :: Ord a => [a] -> [a]
quicksort x = 
  case x of
    [] -> []
    (h:t) -> myappend (myappend (quicksort (filterSmaller x h)) [h])  (quicksort (filterLarger x h))

--quicksort [8,9,2,3,1] == [1,2,3,8,9]
--quicksort [] == []
--quicksort [1] == [1]
--quicksort [1,-1,100,90,1000,-10] == [-10,-1,1,90,100,1000]

-- Exercise 19
evensB :: [Integer] -> [Integer]
evensB l | (l == []) = []
         | otherwise = [x | x <- l, x `mod` 2 == 0]

--evensB [1,2,3,4,5] == [2,4]
--evensB [1,1,1,1] == []


-- Exercise 20

--map examples:

mapDouble :: [Integer] -> [Integer]
mapDouble n = map (\x -> x * 2) n

--mapDouble [1,2,3] == [2,4,6]
--mapDouble [] == []

mapIncrementBy :: Integer -> [Integer] -> [Integer]
mapIncrementBy l n = map (\x -> x + l) n

--mapIncrementBy 3 [1,2,3] == [4,5,6]
--mapIncrementBy 1 [1,2,3] == [2,3,4]

mapDecrementBy :: Integer -> [Integer] -> [Integer]
mapDecrementBy l n = map (\x -> x - l) n

--mapDecrementBy 3 [1,2,3] == [-2,-1,0]
--mapDecrementBy 1 [1,2,3] == [0,1,2]


mymap :: (a -> b) -> [a] -> [b]
mymap fun l = 
  case l of
    [] -> []
    (h:t) -> fun h : mymap fun t

--mymap (\x -> x - 1) [5,6,7] == [4,5,6]
--mymap (\x -> x - 1) [] == []
--mymap (\x -> x * 2) [1,2,3] == [2,4,6]

-- Exercise 21
twice :: (a -> a) -> a -> a
twice fun l = (fun . fun) (l)

--twice (\x -> x - 1) 2 == 0
--twice (\x -> x * 2) 3 == 12

-- Exercise 22
compose :: (b -> c) -> (a -> b) -> a -> c
compose fun1 fun2 l = (fun1 . fun2) (l)

--compose (\x -> x * 5) (\x -> x + 4) 7 == 55
--compose (\x -> x + 4) (\x -> x * 5) 7 == 39
--compose (\x -> x - 1) (\x -> x * 10) 5 == 49
--compose (\x -> x * 10) (\x -> x - 1) 5 == 40

-- Exercise 23
mylast :: [a] -> a
mylast l = 
  case l of
    [] -> error "NO LAST"
    (h:t) -> if (length l == 1) then (head l) else head(myreverse l)

--mylast [1,2,3,4,5,6,7] == 7
--mylast [6,5,4,3] == 3

-- Exercise 24
mylastb :: [a] -> a
mylastb l = 
  case l of
    [] -> error "NO LAST"
    (h:t) -> if (length l == 1) then (head l) else head((drop((length l) - 1)) l)

--mylastb [1,2,3,4,5,6,7] == 7
--mylastb [6,5,4,3] == 3

-- Exercise 25
myinit, myinitb :: [a] -> [a]
myinit l = 
  case l of
    [] -> []
    (h:t) ->  if (length l == 1) then [] else take((length l) - 1) l

myinitb l = 
  case l of
    [] -> []
    (h:t) -> if (length l == 1) then [] else h:myinitb (tail(l))

--myinit [1,2,3,4] == [1,2,3]
--myinit [1] == []
--myinitb [1,2,3,4] == [1,2,3]
--myinitb [1] == []

-- Exercise 26
mysecondconcat :: [[a]] -> [a]
mysecondconcat l = 
  case l of
    [] -> []
    (h:t) -> foldr (++) [] l

--mysecondconcat [[3,4,5][6,7,8][9,0,1]] == [3,4,5,6,7,8,9,0,1]
--mysecondconcat [[10,9,8][7,6,5]] == [10,9,8,7,6,5]

mysecondreverse :: [a] -> [a]
mysecondreverse l = 
  case l of
    [] -> []
    (h:t) -> foldr (\x y -> y ++ [x]) [] l

--mysecondreverse [9,8,7,6,5,4,3,2,1] == [1,2,3,4,5,6,7,8,9]
--mysecondreverse [5,4,6] == [6,4,5]

-- Exercise 27
prefix :: [a] -> [[a]]
prefix l = 
  case l of 
    [] -> [[]]
    (h:t) -> l : prefix (myinit l)

--prefix [1,2,3] == [[1,2,3],[1,2],[1],[]]
--prefix [] == [[]]
