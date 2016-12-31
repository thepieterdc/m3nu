{-|
Module      : Types
Description : Contains all datatypes and classes used in the project.
Copyright   : (c) Pieter De Clercq, 2016
License     : MIT
Maintainer  : piedcler.declercq@ugent.be
-}
module Types(module Types, module X) where

import Control.Applicative as X
import Control.Monad as X
import Control.Monad.Trans.State.Lazy as X
import qualified Data.Map as Map
import Data.Maybe

import qualified MBotPlus as Bot
import Utils

-- |The type for Parsers.
newtype Parser a = Parser (String -> [(a, String)])

-- |Applies a parser.
apply :: Parser a -- ^ The parser to apply
      -> String -- ^ The string to apply the parser on
      -> [(a, String)] -- ^ The parsed expression and the remaining string
apply (Parser f) = f

-- |The functor instance for a Parser.
instance Functor Parser where
  fmap = liftM

-- |The Applicative instance for a Parser.
instance Applicative Parser where
  pure = return
  (<*>) = ap

-- |The Monad instance for a Parser.
instance Monad Parser where
  return x = Parser (\s -> [(x, s)])
  m >>= k = Parser (\s -> [(y, u) |
                           (x, t) <- apply m s,
                           (y, u) <- apply (k x) t])

-- |The Alternative instance for a Parser.
instance Alternative Parser where
  empty = mzero
  (<|>) p q = Parser (\s ->
                      case apply p s of
                        [] -> apply q s
                        res -> res)
  some p = do { x <- p; xs <- many p; return (x:xs)}
  many p = some p `mplus` return []

-- |The MonadPlus instance for a Parser.
instance MonadPlus Parser where
  mzero = Parser (const [])
  mplus m n = Parser (ap ((++) . apply m) (apply n))

-- |Expressions.
data Exp = Constant Double -- ^ A literal/constant number
         | Variable String -- ^ A variable name
         | Binary BinaryOp Exp Exp -- ^ A binary expression
         | Unary UnaryOp Exp -- ^ A unary expression
         | Boolean BooleanOp Exp Exp -- ^ A boolean expression
         | Relational RelationalOp Exp Exp -- ^ A relational binary expression
         | RobotLineSensor -- ^ A statement to read te MBot linesensor
         | RobotUltrason -- ^ A statement to read the MBot ultrasonic sensor
         deriving (Eq, Show)

-- |Binary operators.
data BinaryOp = Add -- ^ Add 2 expressions
              | Minus -- ^ Substract 2 expressions
              | Multiply -- ^ Multiply 2 expressions
              | Divide -- ^ Divide 2 expressions
              deriving (Eq, Show)

-- |Boolean operators.
data BooleanOp = And -- ^ Logic AND operator
               | Or -- ^ Logic OR operator
               deriving (Eq, Show)

-- |Colors consist of 3 expressions: red, green, blue respectively.
type Color = (Exp, Exp, Exp)

-- |Unary operators.
data UnaryOp = Abs -- ^ Absolute value of an expression
             | Not -- ^ Boolean negate an expression
            deriving (Eq, Show)

-- |Relational operators.
data RelationalOp = Greater -- ^ Strict greater than (>)
                  | GrEquals -- ^ Greater than or equals (>=)
                  | Equals -- ^ Equals (==)
                  | LtEquals -- ^ Less than or equals (<=)
                  | Less -- ^ Strict less than (<)
                  deriving (Eq, Show)

-- |Statements.
data Statement = Cook Exp -- ^ A sleep statement
               | Eating Exp Statement -- ^ A while statement
               | Hungry Exp Statement Statement -- ^ A conditional statement
               | Order String Exp -- ^ An assignment statement
               | Puke Exp -- ^ A print statement
               | Review -- ^ Comments
               | RobotDrive Bot.Direction -- ^ MBot command: motors
               | RobotLeds Bot.Led Color -- ^ MBot command: LEDs
               | Seq [Statement] -- ^ A sequence of multiple statements
               deriving Show

-- |An environment variable.
type EnvironmentVar = Map.Map String Double

-- |An environment; built upon a State/IO Transformer.
type Environment a = StateT EnvironmentVar IO a

-- |Retrieves an environment variable.
environmentGet :: String -> Environment Double
environmentGet k = fmap (fromJust . Map.lookup k) get

-- |Sets an environment variable.
environmentSet :: String -> Double -> Environment ()
environmentSet k v = put . Map.insert k v =<< get
