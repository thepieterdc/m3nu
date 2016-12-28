module Utils(module Utils) where

import Data.Maybe

-- converts bool to double-- converts bool to double
boolDouble :: Bool -> Double
boolDouble x = if x then 1 else 0

-- converts double to bool
doubleBool :: Double -> Bool
doubleBool = (/= 0)

-- converts double to int
doubleInt :: Double -> Int
doubleInt = round

-- converts int to double
intDouble :: Int -> Double
intDouble x = fromIntegral x :: Double

-- generates a readsPrec
-- p = possibilities
getReadsPrec :: [(String, a)] -> Int -> String -> [(a, String)]
getReadsPrec p _ s = [(fromJust $ lookup s p, "")]
