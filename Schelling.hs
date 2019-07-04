{- File: Schelling.hs
   Copyright: (C) Mahjeed Marrow 2019
   Description: Defines data types and functions
   that comprise variant of Schelling Model for
   Housing Segregation
-}

module Schelling (
  Home (..),
  City (..),
  initCity,
  openHomes,
  getCoords,
  newLocation,
  oldOcc,
  newOcc,
  satScore,
  getSpot,
  getStatus,
  getIndex,
  deleteAt,
  insertAt,
  percentSat,
  getNeighbors
) where

-- ************************************
-- BEGIN CREATE DATA TYPES
-- ************************************
{- type alias representing a home
"occupant" can be R (Red), B (Blue), or O (Open) -}
type Home = ((Int, Int), String)

{- data type representing a City -}
data City a = City {nRows  :: Int,
                    nCols  :: Int,
                    houses :: [Home]}
                    deriving (Show, Eq)

{- data type representing all available homes -}
newtype Unoccupied a = Unoccupied {homes :: [Home]} deriving (Show)
-- ************************************
-- END CREATE DATA TYPES
-- ************************************

-- ************************************
-- BEGIN CREATE DATA REPRESENTATIONS
-- ************************************
{- creates a City given number of rows and columns -}
initCity :: Int -> Int -> [String] -> City a
initCity x y z = City {nRows     = x,
                       nCols     = y,
                       houses    = zip coords statuses}
                       where coords   = [(a,b) | a <- [0..x-1], b <- [0..y-1]]
                             statuses = [c     | c <- z]

{- takes a City and returns all of the available homes in it -}
openHomes :: [Home] -> [Home]
openHomes homes = filter (\x -> (snd x) == "O") homes
-- ************************************
-- END CREATE DATA REPRESENTATIONS
-- ************************************

-- ************************************
-- BEGIN SOME UTILITY FUNCTIONS
-- ************************************
{- returns occupancy status of a Home -}
getStatus :: Home -> String
getStatus (a,b) = b

{- gets the coordinates of an instance of a Home type -}
getCoords :: Home -> (Int, Int)
getCoords (a,b) = a
-- ************************************
-- END UTILITY FUNCTIONS
-- ************************************

-- ************************************
-- BEGIN RELOCATION LOGIC
-- ************************************
{- calculates satisfaction score for a
house given radius and city -}
satScore :: [Home] -> Int -> Home -> Float
satScore homes r house
          | (snd house) == "O" = (fromIntegral 0)
          | otherwise          = ratio
  where ratio = 100 * ((fromIntegral same) / (fromIntegral total))
        same  = length (filter (\x -> (snd x) == (snd house)) (getNeighbors homes r house))
        total = length (filter (\x -> (snd x) /= "O") (getNeighbors homes r house))

{- removes a house from a list of homes -}
deleteAt :: Int -> [Home] -> [Home]
deleteAt idx xs = lft ++ rgt
            where (lft, (_:rgt)) = splitAt idx xs

{- inserts a house into a list of homes -}
insertAt :: Int -> Home  -> [Home] -> [Home]
insertAt idx x xs = lft ++ [x] ++ rgt
            where (lft, rgt) = splitAt idx xs

{- takes list of homes and
a threshold value and returns the Home
with the min value that is above the threshold -}
newLocation :: [Home] -> Home -> Int -> Int -> Home
newLocation homes house r t = getSpot homes candidates r (minimum qualified)
                    where candidates = map (newOcc house) (openHomes homes)
                          qualified  = map (satScore homes r) candidates

{- helper function for newLocation -}
getSpot :: [Home] -> [Home] -> Int -> Float -> Home
getSpot homes (x:xs) r score
        | (satScore homes r x) == score = x
        | otherwise = getSpot homes xs r score

{- takes a list of homes, a radius, and
a threshold and returns True if at least
one house is unsatisfies, False otherwise -}
anyUnsat :: [Home] -> Int -> Int -> Bool
anyUnsat homes radius thresh
  | (length unsats) > 0 = True
  | otherwise           = False
  where unsats   = [x | x <- scores, x < (fromIntegral thresh)]
        scores   = map (satScore homes radius) occHomes
        occHomes = [x | x <- homes, (snd x) /= "O"]

{- calculates the percentage of satisfied residents -}
percentSat :: [Home] -> Int -> Int -> Int
percentSat homes r t = round ((satLength / homeLength) * 100)
           where sats       = [x | x <- scores, x >= (fromIntegral t)]
                 scores     = map (satScore homes r) occHomes
                 occHomes   = [x | x <- homes, (snd x) /= "O"]
                 satLength  = fromIntegral $ length sats
                 homeLength = fromIntegral $ length homes

{- takes two homes and returns a new
home with the destination location and
the source occupant -}
newOcc :: Home -> Home -> Home
newOcc ((_,_), c) ((a,b), _) = ((a,b), c)

{- takes two homes and returns a new
home with the source location and
updated occupancy to open ("O") -}
oldOcc :: Home -> Home -> Home
oldOcc ((a,b), _) ((_,_), _) = ((a,b), "O")

{- returns the list index of a given house -}
getIndex :: City a -> Home -> Int
getIndex city ((a,b),_) = (nCols city * a) + b
-- ************************************
-- END RELOCATION LOGIC
-- ************************************

-- ************************************
-- BEGIN GET NEIGHBORS
-- ************************************
{- returns list of how neighbors of a given home
given a radius r -}
getNeighbors :: [Home] -> Int -> Home -> [Home]
getNeighbors homes r house = filter (isNeighbor r house) homes

{- compares two houses and returns True if they are
neighbors, False otherwise -}
isNeighbor :: Int -> Home -> Home -> Bool
isNeighbor r h1 h2
          | abs ((fst home1) - (fst home2)) <= r &&
            abs ((snd home1) - (snd home2)) <= r = True
          | otherwise                            = False
          where home1 = getCoords h1
                home2 = getCoords h2
-- ***********************************
-- END GET NEIGHBORS
-- ***********************************
