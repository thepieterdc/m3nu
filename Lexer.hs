module Lexer where

import Data.Char

import Types

-- parses between two delims
between :: Char -> Char -> Parser a -> Parser a
between l r p = do { _ <- token l; ret <- p; _ <- token r; _ <- whitespace; return ret}

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
endline :: Parser Char
endline = do { _ <- some semicolon; _ <- whitespace; return ';'}

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
parens = between '(' ')'

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

-- [ OTHER FUNCTIONS ] --
trim :: String -> String
trim = filter (/= '\n') . filter (/= '\r')

-- [ TOKENIZERS ] --

-- parses a binary expression -- todo add bools
tokenizeBinExp :: Parser Exp
tokenizeBinExp = add <|> sub <|> mul <|> dvd where
  add = do { ret <- parens $ bin '+' Add; _ <- whitespace; return ret }
  sub = do { ret <- parens $ bin '-' Minus; _ <- whitespace; return ret }
  mul = do { ret <- parens $ bin '*' Multiply; _ <- whitespace; return ret }
  dvd = do { ret <- parens $ bin '/' Divide; _ <- whitespace; return ret }
  bin tk op = do { x <- tokenizeExp; _ <- token tk; y <- tokenizeExp; _ <- whitespace; return $ Binary op x y}

-- parses a boolean
tokenizeBool :: Parser Exp
tokenizeBool = true <|> false where
  true = do { _ <- string "tasty"; _ <- whitespace; return $ Constant 1 }
  false = do { _ <- string "disguisting"; _ <- whitespace; return $ Constant 0 }

-- parses an expr
tokenizeExp :: Parser Exp
tokenizeExp = parens tokenizeBool <|> tokenizeBool
            <|> parens num <|> num <|> var
            <|> tokenizeBinExp <|> tokenizeUnaryExp <|> tokenizeRelExp where
  num = do { n <- tokenizeNumber; _ <- whitespace; return $ Constant n }
  var = do { x <- some (spot isAlphaNum); _ <- whitespace; return $ Variable x }

-- parses a double number
tokenizeNumber :: Parser Double
tokenizeNumber = float <|> negFloat <|> nat <|> negNat where
  float = do { n <- digits; dot <- token '.'; f <- digits; return $ read (n ++ [dot] ++ f)}
  nat = do { s <- digits; return $ read s}
  negFloat = do { _ <- token '-'; n <- float; return $ -n}
  negNat = do { _ <- token '-'; n <- nat; return $ -n}

tokenizeRelExp :: Parser Exp
tokenizeRelExp = gt <|> lt <|> eq where
  gt = do { ret <- parens $ rel ">" Greater; _ <- whitespace; return ret }
  lt = do { ret <- parens $ rel "<" Less; _ <- whitespace; return ret }
  eq = do { ret <- parens $ rel "==" Equals; _ <- whitespace; return ret }
  rel tk op = do { x <- tokenizeExp; _ <- string tk; _ <- whitespace; y <- tokenizeExp; _ <- whitespace; return $ Relational op x y}

-- parses a Unary expression
tokenizeUnaryExp :: Parser Exp
tokenizeUnaryExp = parens absval <|> absval where
  absval = do { ret <- between '|' '|' tokenizeExp; return $ Unary Abs ret }

-- skips until a given token, returning the skipped part including the cond obv because parsed
tokenizeUntil :: Parser a -> Parser String
tokenizeUntil cond = done <|> oncemore where
  done = do { _ <- cond; return ""}
  oncemore = do { c <- char; b <- tokenizeUntil cond; return $ c : b}
