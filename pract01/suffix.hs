-- functionName of type Int with a result type of Int
square :: Int -> Int
square n = n * n

-- constantName of type Float with the value of 22/7 which gives us 3.142857
piApprox :: Float
piApprox = 22/7

-- here we have two Int parameters
twiceSum :: Int -> Int -> Int
twiceSum x y = 2 * (x + y)


-- exercises

timesTen :: Int -> Int
timesTen n = n * 10

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle r = piApprox * r^2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder l r = areaOfCircle r * l

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt ((y1 - y2)^2 + (x1 - x2)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = (x /= y && x /= z) && (y/= x && y/= z)

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0 -- `backticks` make a function name into an infix operator

isEven :: Int -> Bool
isEven n = divisibleBy n 2

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

absolute :: Int -> Int
absolute n = if n < 0 then n * (-1) else n

-- playing

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))

-- numDigits :: Int
-- numDigits = length (show reallyBig)

numDigits :: Int -> Int
numDigits n = length(show n)

d1, d2 :: Double
d1 = 4.5387
d2 = 6.2831e-4

c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3

isEven2 :: Integer -> Bool
isEven2 n
  | n `mod` 2 == 0 = True
  | otherwise = False

-- Pairs
p :: (Int, Char)
p = (3, 'x')

f :: Int -> Int -> Int -> Int
f x y z = x + y + z
