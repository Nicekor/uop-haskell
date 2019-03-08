{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

double = multiply 2

doubleAll = map (*2)
areDigits = map isDigit

keepPositive = filter (>0)
keepDigits = filter isDigit
keepEvens = filter isEven


addUp :: Num a => [a] -> a
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- worksheet

-- use of map, filter and foldr
mult10 = map (*10)

onlyLowerCase = filter isLower

orAll bs = foldr (||) False bs

sumSquares = addUp . map(^2)

zeroToTen = filter (>=0) . filter (<=10)

squareRoots = map (sqrt) . filter (>0)

countBetween x y = length . filter(>=x) . filter(<=y)
