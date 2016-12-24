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

-- parsers a letter
letter :: Parser Char
letter = spot isAlpha

-- parses multiple letters
letters :: Parser String
letters = some letter

-- parses a lowercase letter
lower :: Parser Char
lower = spot isLower

-- parses a semicolon
semicolon :: Parser Char
semicolon = do { ret <- token ';'; _ <- whitespace; return ret }

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
string s = do { ret <- mapM token s; _ <- whitespace; return ret }

-- matches a tab character
tab :: Parser Char
tab = spot (== '\t')

-- matches multiple tabs
tabs :: Parser String
tabs = some tab

-- matches a given char
token :: Char -> Parser Char
token c = do { ret <- spot (== c); _ <- whitespace; return ret }

-- matches an uppercase letter
upper :: Parser Char
upper = spot isUpper

-- parses whitespace
whitespace :: Parser String
whitespace = many space

-- [ TOKENIZERS ] --
tokenizeString :: Parser String
tokenizeString = do { _ <- token '"'; s <- many (spot (/= '"')); _ <- token '"'; return s}
