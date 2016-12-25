module Environment where

import qualified Data.Map as Map

type Environment = Map.Map String Double

newEnvironment :: Environment
newEnvironment = Map.fromList []

getVariable :: String -> Environment -> Maybe Double
getVariable = Map.lookup

setVariable :: String -> Double -> Environment -> Environment
setVariable = Map.insert
