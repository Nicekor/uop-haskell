toDigits :: Integer -> [Integer]
toDigits n = if n <= 0 then [] else toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = if n <= 0 then [] else n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (toDigitsRev n) = map (*2) n
