--
-- MATHFUN
-- UP876126
--
import Data.List
import Data.Ord
--
-- Types
--
data Album = Album String String Int Int
  deriving (Show, Read)

testData :: [Album]
testData = [
  (Album "Greatest Hits" "Queen" 1981 6300000),
  (Album "Gold: Greatest Hits" "ABBA" 1992 5400000),
  (Album "Sgt. Pepper's Lonely Hearts Club Band" "The Beatles" 1967 5340000),
  (Album "21" "Adele" 2011 5110000),
  (Album "(What's the Story) Morning Glory?" "Oasis" 1995 4940000),
  (Album "Thriller" "Michael Jackson" 1982 4470000),
  (Album "The Dark Side of the Moon" "Pink Floyd" 1973 4470000),
  (Album "Brothers in Arms" "Dire Straits" 1985 4350000),
  (Album "Bad" "Michael Jackson" 1987 4140000),
  (Album "Rumours" "Fleetwood Mac" 1977 4090000),
  (Album "Greatest Hits II" "Queen" 1991 3990000),
  (Album "Back to Black" "Amy Winehouse" 2006 3940000),
  (Album "The Immaculate Collection" "Madonna" 1990 3700000),
  (Album "25" "Adele" 2015 3500000),
  (Album "Stars" "Simply Red" 1991 3450000),
  (Album "Come On Over" "Shania Twain" 1998 3430000),
  (Album "x" "Ed Sheeran" 2014 3380000),
  (Album "Legend" "Bob Marley" 1984 3380000),
  (Album "Bat Out of Hell" "Meat Loaf" 1977 3370000),
  (Album "Back to Bedlam" "James Blunt" 2004 3360000),
  (Album "Urban Hymns" "The Verve" 1997 3340000),
  (Album "Bridge over Troubled Water" "Simon & Garfunkel" 1970 3260000),
  (Album "1" "The Beatles" 2000 3230000),
  (Album "Spirit" "Leona Lewis" 2007 3170000),
  (Album "Crazy Love" "Michael Bublé" 2009 3130000),
  (Album "No Angel" "Dido" 2000 3090000),
  (Album "White Ladder" "David Gray" 1998 3020000),
  (Album "The Fame" "Lady Gaga" 2009 2990000),
  (Album "Only by the Night" "Kings of Leon" 2008 2980000),
  (Album "A Rush of Blood to the Head" "Coldplay" 2002 2960000),
  (Album "Talk on Corners" "The Corrs" 1997 2960000),
  (Album "Spice" "Spice Girls" 1996 2960000),
  (Album "Life for Rent" "Dido" 2003 2900000),
  (Album "Beautiful World" "Take That" 2006 2880000),
  (Album "The Joshua Tree" "U2" 1987 2880000),
  (Album "Hopes and Fears" "Keane" 2004 2860000),
  (Album "The War of the Worlds" "Jeff Wayne" 1978 2800000),
  (Album "X&Y" "Coldplay" 2005 2790000),
  (Album "Jagged Little Pill" "Alanis Morissette" 1995 2780000),
  (Album "Tubular Bells" "Mike Oldfield" 1973 2760000),
  (Album "Scissor Sisters" "Scissor Sisters" 2004 2760000),
  (Album "...But Seriously" "Phil Collins" 1989 2750000),
  (Album "Tracy Chapman" "Tracy Chapman" 1988 2710000),
  (Album "Parachutes" "Coldplay" 2000 2710000),
  (Album "The Man Who" "Travis" 1999 2687500),
  (Album "Greatest Hits" "ABBA" 1975 2606000),
  (Album "I've Been Expecting You" "Robbie Williams" 1998 2586500),
  (Album "Come Away with Me" "Norah Jones" 2002 2556650),
  (Album "Graceland" "Paul Simon" 1986 2500000),
  (Album "Ladies & Gentlemen: The Best of" "George Michael" 1998 2500000)
  ]

-- Functional code

-- ######################### HELPER FUNCTIONS #########################
-- Functions to get properties of the datatype
title :: Album -> String
title (Album title _ _ _) = title

artist :: Album -> String
artist (Album _ artist _ _) = artist

year :: Album -> Int
year (Album _ _ year _) = year

sales :: Album -> Int
sales (Album _ _ _ sales) = sales

-- Display a single album
displayAlbum :: Album -> String
displayAlbum album =
  title album
  ++ " | "
  ++ artist album
  ++ " | "
  ++ show (year album)
  ++ " | "
  ++ show (sales album)

-- for exercise (ii) and vii
orderAlbumsPerSales :: [Album] -> [Album]
orderAlbumsPerSales albums = sortBy (comparing (\x -> Down (sales x))) albums

-- for exercise (vii)
groupAlbumsByArtist :: [Album] -> [[Album]]
groupAlbumsByArtist albums = groupBy (\x y -> artist x == artist y) (sortBy (comparing artist) albums)

-- for exercise (vii) UI
top50ToString :: [(String, Int)] -> String
top50ToString [] = ""
top50ToString [x] = fst x ++ " - " ++ show (snd x)
top50ToString (x:xs) = fst x ++ " - " ++ show (snd x) ++ "\n" ++ top50ToString xs

-- ##################### END OF HELPER FUNCTIONS #####################

-- exercise (i)
albumsToString :: [Album] -> String
albumsToString [] = ""
albumsToString [x] = displayAlbum x
albumsToString (x:xs) = displayAlbum x ++ "\n" ++ albumsToString xs

-- exercise (ii)
top10 :: [Album] -> [Album]
top10 albums = take 10 (orderAlbumsPerSales albums)

-- exercise (iii)
albumsReleasedBetween :: [Album] -> Int -> Int -> [Album]
albumsReleasedBetween albums from to = filter (\x -> (year x >= from && year x <= to)) albums

-- exercise (iv)
albumsBegginingWith :: [Album] -> String -> [Album]
albumsBegginingWith albums prefix = filter (\x -> (prefix `isPrefixOf` title x)) albums

-- exercise (v)
-- The filter will give all the albums of the right artist, and the map will get the sales numbers of those
totalSalesOf :: [Album] -> String -> Int
totalSalesOf albums artist' = foldr (+) 0 (map sales (filter (\x -> artist x == artist') albums))

-- exercise (vi)
timesInTop50 :: [Album] -> [(String, Int)]
timesInTop50 albums = map (\x -> (artist (x!!0), length x)) (groupAlbumsByArtist albums)

-- exercise (vii)
addAlbum :: [Album] -> String -> String -> Int -> Int -> [Album]
addAlbum [a] title' artist' year' sales' = if sales a >= sales'
  then [Album title' artist' year' sales']
  else [a]
addAlbum albums title' artist' year' sales' = if sales' >= sales (head (orderAlbumsPerSales albums))
  then (Album title' artist' year' sales') : init (orderAlbumsPerSales albums)
  else head (orderAlbumsPerSales albums) : addAlbum (tail (orderAlbumsPerSales albums)) title' artist' year' sales'

-- exercise (viii)
increaseSales :: [Album] -> String -> String -> Int -> [Album]
increaseSales [album] title' artist' amount = if title album == title' && artist album == artist'
  then [Album (title album) (artist album) (year album) (sales album + amount)]
  else []
increaseSales (x:xs) title' artist' amount = if title x == title' && artist x == artist'
  then orderAlbumsPerSales (Album (title x) (artist x) (year x) (sales x + amount) : xs)
  else orderAlbumsPerSales (x : increaseSales xs title' artist' amount)


-- Demo function to test basic functionality (without persistence - i.e.
-- testData doesn't change and nothing is saved/loaded to/from albums file).

demo :: Int -> IO ()
demo 1  = putStrLn (albumsToString testData)
demo 2  = putStrLn (albumsToString (top10 testData))
demo 3  = putStrLn (albumsToString (albumsReleasedBetween testData 2000 2008))
demo 4  = putStrLn (albumsToString (albumsBegginingWith testData "Th"))
demo 5  = putStrLn (show (totalSalesOf testData "Queen"))
demo 6  = putStrLn (top50ToString (timesInTop50 testData))
demo 7  = putStrLn (albumsToString (addAlbum testData "Progress" "Take That" 2010 2700000))
demo 8  = putStrLn (albumsToString (increaseSales testData "21" "Adele" 400000))



-- User interface (and loading/saving) code

loadAlbums :: IO [Album]
loadAlbums = do
  contents <- readFile "albums.txt"
  return (read contents :: [Album])

startMenu :: [Album] -> IO ()
startMenu albumsList = do
  putStrLn ""
  putStrLn "Enter the number of the option desired: "
  putStrLn "1. Display the 50 bestselling albums in the UK."
  putStrLn "2. Display the top 10 albums in descending order of sales."
  putStrLn "3. Display all albums that were released between two given years (inclusive)."
  putStrLn "4. Display all albums by a given title prefix."
  putStrLn "5. Display the total sales of a given artist."
  putStrLn "6. Display a list of artists with the number of albums they have in the top 50."
  putStrLn "7. Add a new album."
  putStrLn "8. Increase the sales of one album given its title and artist."
  putStrLn "0. Exit"
  option <- getLine
  executeOption option albumsList

executeOption :: String -> [Album] -> IO ()
executeOption "1" albumsList = do
  putStrLn (albumsToString albumsList)
  startMenu albumsList
executeOption "2" albumsList = do
  putStrLn (albumsToString (top10 albumsList))
  startMenu albumsList
executeOption "3" albumsList = do
  putStrLn "From what year?"
  fromStr <- getLine
  let from = read fromStr :: Int
  putStrLn "Until what year"
  toStr <- getLine
  let to = read toStr :: Int
  let filteredYears = albumsReleasedBetween albumsList from to
  putStrLn (albumsToString filteredYears)
  startMenu albumsList
executeOption "4" albumsList = do
  putStrLn "Enter the title prefix:"
  prefix <- getLine
  let filteredAlbums = albumsBegginingWith albumsList prefix
  putStrLn (albumsToString filteredAlbums)
  startMenu albumsList
executeOption "5" albumsList = do
  putStrLn "Enter the artist/band:"
  artist <- getLine
  let totalSales = totalSalesOf albumsList artist
  putStrLn (show totalSales)
  startMenu albumsList
executeOption "6" albumsList = do
  putStrLn (top50ToString (timesInTop50 albumsList))
  startMenu albumsList
executeOption "7" albumsList = do
  putStrLn "Enter the title:"
  title <- getLine
  putStrLn "Enter the artist:"
  artist <- getLine
  putStrLn "Enter the year:"
  yearStr <- getLine
  let year = read yearStr :: Int
  putStrLn "Enter the sales:"
  salesStr <- getLine
  let sales = read salesStr :: Int
  let newAlbumsList = addAlbum albumsList title artist year sales
  putStrLn "Album successfuly added."
  startMenu newAlbumsList
executeOption "8" albumsList = do
  putStrLn "Enter the title:"
  title <- getLine
  putStrLn "Enter the artist:"
  artist <- getLine
  putStrLn "Enter the additional sales:"
  salesStr <- getLine
  let sales = read salesStr :: Int
  let newAlbumsList = increaseSales albumsList title artist sales
  putStrLn "Sales successfuly increased"
  startMenu newAlbumsList
executeOption "0" albumsList = do
  writeFile "albums.txt" (show albumsList)
executeOption _ albumsList = do
  putStrLn "Select a valid option."
  startMenu albumsList

-- UI
main :: IO ()
main = do
  albumsList <- loadAlbums
  putStrLn "The 50 bestselling albums in the UK:\n"
  putStrLn (albumsToString albumsList)
  startMenu albumsList
