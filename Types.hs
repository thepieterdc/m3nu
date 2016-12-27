module Types (module Types, module Control.Applicative, module Control.Monad) where

import Control.Applicative
import Control.Monad

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

doParse :: (Show a) => Parser a -> String -> a
doParse m s = one [x | (x,t) <- apply m s, t == "" ] where
  one [x] = x
  one [] = error "Parse not completed."
  one xs | length xs > 1 = error ("Multiple parses found:\n " ++ show xs)
  one _ = error "Unknown parse error."

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
         deriving Show

data BinaryOp = And | Or | Add | Minus | Multiply | Divide deriving Show

data UnaryOp = Abs deriving Show

data RelationalOp = Greater | Equals | Less deriving Show

data Statement = Cook Exp
               | Debug String
               | Eating Exp Statement
               | Hungry Exp Statement Statement
               | Order String Exp
               | Puke Exp
               | Review
               | Seq [Statement]
               deriving Show
