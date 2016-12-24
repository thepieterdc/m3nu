module Types (module Types, module Control.Applicative, module Control.Monad) where

import Control.Applicative
import Control.Monad

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

data BoolExp = BoolConst Bool
             | BoolNegate BoolExp
             | BoolBinary BoolBinaryOp BoolExp BoolExp
             | RelationalBinary RelationalBinaryOp ArithExp ArithExp
             deriving Show

data BoolBinaryOp = And | Or deriving Show

data RelationalBinaryOp = Greater | Less | Equals deriving Show

data ArithExp = Variable String
              | ArithConst Double
              deriving Show

data Statement = Order String ArithExp deriving Show
