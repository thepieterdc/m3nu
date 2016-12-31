{-|
Module      : Utils
Description : Provides simple utility functions.
Copyright   : (c) Pieter De Clercq, 2016
License     : MIT
Maintainer  : piedcler.declercq@ugent.be
-}
module Utils(module Utils) where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

-- |Converts a Boolean to a Double value.
boolDouble :: Bool -> Double
boolDouble True = 1
boolDouble False = 0

{-|
  Converts a Double to a Boolean value. Every number is converted to True except
  for zero.
-}
doubleBool :: Double -> Bool
doubleBool = (/= 0)

-- |Converts a Double to an Integer.
doubleInt :: Double -> Int
doubleInt = round

-- |Converts a floating point number to a Double.
floatDouble :: Float -> Double
floatDouble = realToFrac

-- |Retrieves the index of an element in a list, erroring if not found.
index :: (Eq a) => [a] -> a -> Int
index xs x = Maybe.fromJust (List.elemIndex x xs)

-- |Converts an Integer to a Double.
intDouble :: Int -> Double
intDouble x = fromIntegral x :: Double

-- |Retrieves the value corresponding to a key in a list of tuples.
mapLookup :: (Ord a) => [(a,b)] -> a -> Maybe b
mapLookup xs x = Map.lookup x (Map.fromList xs)
