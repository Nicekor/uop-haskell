helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do
    putStr "Enter a line: "
    str <- getLine
    if str == "" then
        return ()
    else do
        putStrLn (isPalindrome str)
        palLines

-- For exercise 6
fahrenheit2Celsius :: Float -> Float
fahrenheit2Celsius f = (f - 32) * 5 / 9

celsius2Fahrenheit :: Float -> Float
celsius2Fahrenheit c = c * 9 / 5 + 32

-- worksheet
greeting :: IO ()
greeting = do
  putStr "What's your name? "
  name <- getLine
  putStrLn ("Hello, " ++ name)

test :: Int -> Int -> Int
test n1 n2 = n1 + n2

addTwoNumbers :: IO ()
addTwoNumbers = do
  putStr "Enter a number: "
  n1 <- getLine
  putStr "Enter the second number: "
  n2 <- getLine
  putStrLn ("The result is: " ++ show ((read n1) + (read n2)))

copyFile :: IO ()
copyFile = do
  putStr "Enter the filename you want to copy: "
  name <- getLine
  contents <- readFile name
  putStr "Enter the name of the new file: "
  copyName <- getLine
  writeFile copyName contents
