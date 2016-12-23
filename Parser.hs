module Parser where

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap = liftM
