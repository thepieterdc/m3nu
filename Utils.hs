module Utils(module Utils) where

import Data.List(elemIndex)
import Data.Maybe(fromJust)
import qualified Data.Map as Map

-- converts bool to double-- converts bool to double
boolDouble :: Bool -> Double
boolDouble True = 1
boolDouble False = 0

-- converts double to bool
doubleBool :: Double -> Bool
doubleBool = (/= 0)

-- converts double to int
doubleInt :: Double -> Int
doubleInt = round

-- converts float to double
floatDouble :: Float -> Double
floatDouble = realToFrac

-- gets the index of an element in a list and errors otherwise
index :: (Eq a) => [a] -> a -> Int
index xs x = fromJust (elemIndex x xs)

-- converts int to double
intDouble :: Int -> Double
intDouble x = fromIntegral x :: Double

-- map.lookup for lists
mapLookup :: (Ord a) => [(a,b)] -> a -> Maybe b
mapLookup xs x = Map.lookup x (Map.fromList xs)
