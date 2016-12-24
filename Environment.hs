module Environment where

import qualified Data.Map as Map

data EnvVal = EnvInt Int deriving (Eq, Show)

type Environment = Map.Map String EnvVal

newEnvironment :: Environment
newEnvironment = Map.fromList []
