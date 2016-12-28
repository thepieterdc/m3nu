module Types(module Types, module Control.Applicative, module Control.Monad) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import MBot

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  m >>= k = Parser (\s -> [(y, u) |
                           (x, t) <- apply m s,
                           (y, u) <- apply (k x) t])

instance Alternative Parser where
  empty = mzero
  (<|>) p q = Parser (\s ->
                      case apply p s of
                        [] -> apply q s
                        res -> res)
  some p = do { x <- p; xs <- many p; return (x:xs)}
  many p = some p `mplus` return []

instance MonadPlus Parser where
  mzero = Parser (const [])
  mplus m n = Parser (\s -> apply m s ++ apply n s)

data Exp = Constant Double
         | Variable String
         | Binary BinaryOp Exp Exp
         | Unary UnaryOp Exp
         | Relational RelationalOp Exp Exp
         | RobotLineSensor
         deriving Show

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

-- converts MBot Line to double
lineDouble :: Line -> Double
lineDouble x = intDouble $ fromJust $ elemIndex x [BOTHW, LEFTB, RIGHTB, BOTHB]

data BinaryOp = And | Or | Add | Minus | Multiply | Divide deriving Show

data UnaryOp = Abs | Not deriving Show

data RelationalOp = Greater | Equals | Less deriving Show

data RobotDirection = Forward | Left | Right | Backward deriving Show

data RobotLed = LeftLed | RightLed deriving Show

data Statement = Cook Exp
               | Debug String
               | Eating Exp Statement
               | Hungry Exp Statement Statement
               | Order String Exp
               | Puke Exp
               | Review
               | RobotLeds RobotLed Exp Exp Exp
               | Seq [Statement]
               deriving Show

type EnvironmentVar = Map.Map String Double

type Environment a = StateT EnvironmentVar IO a

-- fromjust want als var niet bestaat gaat toch niet werken
-- gets variable
environmentGet :: String -> Environment Double
environmentGet k = do { env <- get; return $ fromJust $ Map.lookup k env }

-- sets variable
environmentSet :: String -> Double -> Environment ()
environmentSet k v = do { env <- get; put $ Map.insert k v env; return () }
