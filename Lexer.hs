module Lexer(module Lexer, module Data.Char) where

import Data.Char

import Types

-- parser one char, anything, returns the char + rest of string
char :: Parser Char
char = Parser f where
  f [] = []
  f (c:s) = [(c,s)]

-- parses a digit, returns it as a char for composing numbers
digit :: Parser Char
digit = spot isDigit

-- parses multiple digits, returns them as string for composing numbers
digits :: Parser String
digits = some digit

-- parses the end of a line
endline :: Parser ()
endline = do { _ <- semicolon; _ <- whitespace; return ()}

-- parses an identifier; must always be followed by at least one whitespace
identifier :: String -> Parser String
identifier s = do {i <- string s; _ <- spaces; return i}

-- parsers a letter
letter :: Parser Char
letter = spot isAlpha

-- parses multiple letters
letters :: Parser String
letters = some letter

-- parses a lowercase letter
lower :: Parser Char
lower = spot isLower

-- runs the parser between ( )
parens :: Parser a -> Parser a
parens = tokenizeBetween '(' ')'

-- parses a semicolon
semicolon :: Parser Char
semicolon = token ';'

-- parses a space/newline/tabs
space :: Parser Char
space = spot isSpace

-- parses multiple spaces/newlines/tabs
spaces :: Parser String
spaces = some space

-- matches a given predicate
spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c}

-- matches a string
string :: String -> Parser String
string = mapM token

-- matches a tab character
tab :: Parser Char
tab = token '\t'

-- matches multiple tabs
tabs :: Parser String
tabs = some tab

-- matches a given char
token :: Char -> Parser Char
token c = spot (== c)

-- matches an uppercase letter
upper :: Parser Char
upper = spot isUpper

-- parses whitespace
whitespace :: Parser String
whitespace = many space

-- [ TOKENIZERS ] --

-- parses an arith expr
tokenizeArithExp :: Parser ArithExp
tokenizeArithExp = cst <|> var <|> add <|> sub <|> mul <|> dvd where
  cst = do { num <- tokenizeNumber <|> parens tokenizeNumber; _ <- whitespace; return $ ArithConst num }
  var = do { x <- some (spot isAlphaNum); _ <- whitespace; return $ Variable x }
  add = do { ret <- bin2 '+' Add; _ <- whitespace; return ret }
  sub = do { ret <- bin2 '-' Minus; _ <- whitespace; return ret }
  mul = do { ret <- bin2 '*' Multiply; _ <- whitespace; return ret }
  dvd = do { ret <- bin2 '/' Divide; _ <- whitespace; return ret }
  bin2 tk op = parens $ bin tk op
  bin tk op = do { x <- tokenizeArithExp; _ <- token tk; y <- tokenizeArithExp; _ <- whitespace; return $ ArithBinary op x y}

-- parses between two delims
tokenizeBetween :: Char -> Char -> Parser a -> Parser a
tokenizeBetween l r p = do { _ <- token l; ret <- p; _ <- token r; _ <- whitespace; return ret}

-- parses a bool expr
tokenizeBoolExp :: Parser BoolExp
tokenizeBoolExp = parens tokenizeBoolExp
                   <|> relgt <|> releq <|> rellt
                   <|> binand <|> binor
                   <|> true <|> false
                   where
  true = do { _ <- string "tasty"; _ <- whitespace; return $ BoolConst True }
  false = do { _ <- string "disguisting"; _ <- whitespace; return $ BoolConst False }
  binand = do { x <- nxt; _ <- identifier "and"; y <- nxt; _ <- whitespace; return $ BoolBinary And x y }
  binor = do { x <- nxt; _ <- identifier "or"; y <- nxt; _ <- whitespace; return $ BoolBinary Or x y }
  relgt = do { x <- tokenizeArithExp; _ <- token '>'; _ <- whitespace; y <- tokenizeArithExp; _ <- whitespace; return $ RelationalBinary Greater x y }
  releq = do { x <- tokenizeArithExp; _ <- string "=="; _ <- whitespace; y <- tokenizeArithExp; _ <- whitespace; return $ RelationalBinary Equals x y }
  rellt = do { x <- tokenizeArithExp; _ <- whitespace; _ <- token '<'; _ <- whitespace; y <- tokenizeArithExp; _ <- whitespace; return $ RelationalBinary Less x y }

  nxt = true <|> false <|> tokenizeBoolExp

-- parses a double number
tokenizeNumber :: Parser Double
tokenizeNumber = float <|> negFloat <|> nat <|> negNat where
  float = do { n <- digits; dot <- token '.'; f <- digits; return $ read (n ++ [dot] ++ f)}
  nat = do { s <- digits; return $ read s}
  negFloat = do { _ <- token '-'; n <- float; return $ -n}
  negNat = do { _ <- token '-'; n <- nat; return $ -n}

-- skips until a given token, returning the skipped part including the cond obv because parsed
tokenizeUntil :: Parser a -> Parser String
tokenizeUntil cond = done <|> oncemore where
  done = do { _ <- cond; return ""}
  oncemore = do { c <- char; b <- tokenizeUntil cond; return $ c : b}
