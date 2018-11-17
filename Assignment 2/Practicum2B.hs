module Practicum2B where

{-
Name:           <Name and family name>
VU-net id:      <VU-net id>
Student number: <Student number>
Discussed with: <In case you discussed the exercises with someone else,
                 please mention his/her name(s) explicitly here>
Remarks:        <In case something need special attention,
                 please tell us>
Sources:        <
                https://stackoverflow.com/questions/44965/what-is-a-monad
                https://wiki.haskell.org/Monad
                https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/pattern-and-guard-extensions
                https://wiki.haskell.org/MultiCase
                https://en.wikibooks.org/wiki/Haskell/Control_structures
                http://learnyouahaskell.com/a-fistful-of-monads
                https://en.wikibooks.org/wiki/Haskell/Understanding_monads
                https://cs.stackexchange.com/questions/27808/haskell-monad-bind-operator-type
                https://en.wikibooks.org/wiki/Haskell/do_notation
                https://stackoverflow.com/questions/48177824/how-does-return-statement-work-in-haskell
                https://blog.ramdoot.in/monadic-do-block-yet-again-a98cf0237b25
                http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html
                http://book.realworldhaskell.org/read/monads.html
                >
-}

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)


-- given: definition of the monad MaybeOne used for partial functions
data MaybeOne a = NoResult
                | Result a deriving (Show, Eq)

instance Applicative MaybeOne where
    pure x = Result x
    (<*>)  = ap

instance Functor MaybeOne where
    fmap = liftM

instance Monad MaybeOne where
  NoResult   >>= _ = NoResult
  (Result n) >>= f = f n
  return x         = Result x


-- Example: partial function for division
myDividedBy :: Double -> Double -> MaybeOne Double
myDividedBy n d =
  if d  == 0
  then NoResult
  else Result (n / d)

-- Exercise 1-a
myIndexOf :: [Double] -> Double -> MaybeOne Int
myIndexOf l n =
  myIndexOfAux l 1
  where
    myIndexOfAux l i =
      case l of
        [] -> NoResult
        (h:t) -> if (h /= n) then myIndexOfAux t (i + 1) else Result (i)  
-- myIndexOf [4,5,6,7,6,7,6] 6 == Result 3
-- myIndexOf [1,2,3] == NoResult

-- Exercise 1-b
myRemainderString :: String -> String -> MaybeOne String
myRemainderString x y = 
  case x of
    [] -> Result (y)
    (h:t) | length y == 0 -> NoResult
          | let prefix = (take (length x) y)
              in prefix == x -> Result (drop (length x) y)
          | otherwise -> NoResult 
-- myRemainderString "abc" "abcd" == Result "d"
-- myRemainderString "abc" "accd" == NoResult
-- myRemainderString "abc" "lalabcd" == NoResult

-- Create an operator for our divide function
n // d = n `myDividedBy` d

-- Example f using case (1/2)
f :: Double -> Double -> Double -> MaybeOne Double
f x y z = case x // y of
    NoResult           -> NoResult
    Result xDividedByy ->
      case xDividedByy // z of
        NoResult   -> NoResult
        Result   r -> Result r

-- Example f using case (2/2)
fShorter :: Double -> Double -> Double -> MaybeOne Double
fShorter x y z = case x // y of
    NoResult           -> NoResult
    Result xDividedByy -> xDividedByy // z

-- Example g using case
g :: Double -> Double -> Double -> Double -> MaybeOne Double
g x y z s =
  case x // y of
    NoResult           -> NoResult
    Result xDividedByy ->
      case y // z of
        NoResult           -> NoResult
        Result xDividedByz ->
          case y // s of
            NoResult           -> NoResult
            Result yDividedBys ->
              case z // s of
                NoResult           -> NoResult
                Result zDividedBys ->
                  let n = yDividedBys + zDividedBys
                      d = xDividedByy - xDividedByz
                  in n // d


-- Exercise 2
v1 :: Double -> Double -> Double -> Double -> MaybeOne Double
v1 x y z s = 
  case x // y of
    NoResult -> NoResult
    Result xDividedByy ->
      case z // s of
        NoResult -> NoResult
        Result zDividedBys ->
          case y // s of
            NoResult -> NoResult
            Result yDividedBys ->
              case z // x of
                NoResult -> NoResult
                Result zDividedByx ->
                  case (xDividedByy) // (zDividedBys - yDividedBys) of
                    NoResult -> NoResult
                    Result leftOperand ->
                      let rightOperand = (yDividedBys + zDividedByx)
                        in Result (leftOperand - rightOperand)
-- v1 1 2 3 4 == Result (-1.5)
-- v1 3 3 3 3 == NoResult
-- v1 2 5 1 5 == Result (-2.0)

-- Example f using >==
fBetter :: Double -> Double -> Double -> MaybeOne Double
fBetter x y z = (x // y) >>= dividedByZ
  where dividedByZ xdividedByy = xdividedByy // z

-- Example f using >= and lambda 
fBetterLambda :: Double -> Double -> Double -> MaybeOne Double
fBetterLambda x y z = (x // y) >>= (\xDividedByy -> xDividedByy // z)

-- Example g using >== and lambda
gBetter :: Double -> Double -> Double -> Double -> MaybeOne Double
gBetter x y z s =
  (x // y) >>=
  (\xDividedByy ->
    (x // z) >>=
    (\xDividedByz ->
      let d = xDividedByy - xDividedByz
      in (y // s) >>=
      (\yDividedBys ->
        (z // s) >>=
        (\zDividedBys ->
          let n = yDividedBys + zDividedBys
          in n // d
        )
      )
    )
  )

-- Exercise 3
v2 :: Double -> Double -> Double -> Double -> MaybeOne Double
v2 x y z s = 
  (x // y) >>=
    (\xDividedByy -> 
      (z // s) >>=
      (\zDividedBys -> 
        (y // s) >>=
        (\yDividedBys -> 
          (z // x) >>=
          (\zDividedByx -> 
            ((xDividedByy) // (zDividedBys - yDividedBys) >>=
              (\leftOperand -> let rightOperand = (yDividedBys + zDividedByx)
                                  in Result (leftOperand - rightOperand)
              )
            )
          )
        )
      )
    )
-- v2 1 2 3 4 == Result (-1.5)
-- v2 3 3 3 3 == NoResult
-- v2 2 5 1 5 == Result (-2.0)

-- Example f using do
fDo :: Double -> Double -> Double -> MaybeOne Double
fDo x y z = do
  xDividedByy <- x // y
  xDividedByy // z

-- Example g using do
gDo :: Double -> Double -> Double -> Double -> MaybeOne Double
gDo x y z s = do
  xDividedByy <- x // y
  xDividedByz <- y // z
  let d = xDividedByy - xDividedByz
  yDividedBys <- y // s
  zDividedBys <- z // s
  let n = yDividedBys + zDividedBys
  n // d

-- Example f using do-return
fPerfect :: Double -> Double -> Double -> MaybeOne Double
fPerfect x y z = do
  xDividedByy <- x // y
  result      <- xDividedByy // z
  return result

-- Example g using do-return
gPerfect :: Double -> Double -> Double -> Double -> MaybeOne Double
gPerfect x y z s = do
  xDividedByy <- x // y
  xDividedByz <- y // z
  let denominator = xDividedByy - xDividedByz
  yDividedBys <- y // s
  zDividedBys <- z // s
  let numerator = yDividedBys + zDividedBys
  result <- numerator // denominator
  return result

-- Exercise 4
v3 :: Double -> Double -> Double -> Double -> MaybeOne Double
v3 x y z s = do
  xDividedByy <- (x // y)
  zDividedBys <- (z // s)
  yDividedBys <- (y // s)
  zDividedByx <- (z // x)
  leftOperand <- (xDividedByy) // (zDividedBys - yDividedBys)
  rightOperand <- Result (yDividedBys + zDividedByx)
  let result = (leftOperand - rightOperand)
  return result
-- v3 1 2 3 4 == Result (-1.5)
-- v3 3 3 3 3 == NoResult
-- v3 2 5 1 5 == Result (-2.0)
