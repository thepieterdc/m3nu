{-|
Module      : Lexer
Description : Contains tokenizing functions and functions for lexical analysis.
Copyright   : (c) Pieter De Clercq, 2016
License     : MIT
Maintainer  : piedcler.declercq@ugent.be
-}
module Lexer(module Lexer, module Data.Char) where

import Data.Char

import qualified MBotPlus as Bot
import Types hiding(optional)

-- [ LEXICOGRAPHIC HELP FUNCTIONS ] --

-- |Runs a parser between two delimiters.
between :: Char -> Char -> Parser a -> Parser a
between l r p = do { token l; ret <- p; token r; return ret}

-- |Runs a parser between two brackets { }.
brackets :: Parser a -> Parser a
brackets = between '{' '}'

{-|
  Parses one character, returning the character and the rest of the unparsed
  string.
-}
char :: Parser Char
char = Parser f where
  f [] = []
  f (c:s) = [(c,s)]

-- |Parses a digit.
digit :: Parser Char
digit = spot isDigit

-- |Parses multiple digits, returning them as a string.
digits :: Parser String
digits = some digit

-- |Parses the end of an instruction, denoted by a semicolon. Discards result.
end :: Parser ()
end = void (some semicolon)

-- |Parses an identifier. Discards the result.
ident :: String -> Parser ()
ident k = void (string k)

-- |Parses an identifier followed by a semicolon. Discards the result.
keyword :: String -> Parser ()
keyword k = string k >> end >> return ()

-- |Parses a letter, returning the letter.
letter :: Parser Char
letter = spot isAlpha

-- |Parses multiple letters, returning them as a string.
letters :: Parser String
letters = some letter

-- |Parses a lowercase letter.
lower :: Parser Char
lower = spot isLower

-- |Parses a string if it is found, returning the empty string otherwise.
optional :: Parser String -> Parser String
optional a = a <|> return []

-- |Runs a parser between parentheses ( ).
parens :: Parser a -> Parser a
parens = between '(' ')'

-- |Parses a semicolon. Discards the result.
semicolon :: Parser ()
semicolon = token ';'

-- |Parses characters as long as a given condition holds true.
skipUntil :: Parser a -> Parser String
skipUntil cond = done <|> oncemore where
  done = cond >> return []
  oncemore = do { c <- char; b <- skipUntil cond; return $ c : b}

-- |Parses a character satisfying a given predicate.
spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c}

-- |Parses a string.
string :: String -> Parser String
string = mapM (spot . (==))

-- |Parses a given character. Discards the result.
token :: Char -> Parser ()
token c = void (spot (c ==))

-- |Parses an uppercase letter.
upper :: Parser Char
upper = spot isUpper

-- |Preprocesses a file for parsing, removing all whitespace.
preprocess :: String -> String
preprocess = filter (`notElem` " \t\n\r")

-- [ TOKENIZERS ] --

-- |Tokenizes a boolean expression.
bool :: Parser Exp
bool = true <|> false where
  true = ident "tasty" >> return (Constant 1)
  false = ident "disguisting" >> return (Constant 0)

-- |Tokenizes a binary expression.
binaryExpr :: Parser Exp
binaryExpr = add <|> sub <|> mul <|> dvd
           <|> booland <|> boolor <|> gteq <|> lteq <|> gt <|> lt <|> eq where
  binpart = constant <|> unaryExpr <|> binaryExpr
  bin tk = do { x <- binpart; ident tk; y <- binpart; return (x,y)}
  aritexp tk op = do { (x,y) <- parens $ bin tk; return $ Binary op x y}
  add = aritexp "+" Add;
  sub = aritexp "-" Minus;
  mul = aritexp "*" Multiply;
  dvd = aritexp "/" Divide;
  boolexp tk op = do { (x,y) <- parens $ bin tk; return $ Boolean op x y}
  booland = boolexp "and" And;
  boolor = boolexp "or" Or;
  relexp tk op = do { (x,y) <- parens $ bin tk; return $ Relational op x y}
  gteq = relexp ">=" GrEquals;
  lteq = relexp "<=" LtEquals;
  gt = relexp ">" Greater;
  lt = relexp "<" Less;
  eq = relexp "==" Equals;

-- |Tokenizes a color.
color :: Parser Color
color = rgb <|> off <|> white <|> red <|> green <|> blue
                <|> cyan <|> yellow <|> magenta where
  rgbpart = do{ret <- expr; token ','; return ret}
  rgb = do {r <- rgbpart; g <- rgbpart; b <- expr; return (r, g, b)}
  off = ident "off" >> return (Constant 0, Constant 0, Constant 0)
  red = ident "red" >> return (Constant 255, Constant 0, Constant 0)
  green = ident "green" >> return (Constant 0, Constant 255, Constant 0)
  blue = ident "blue" >> return (Constant 0, Constant 0, Constant 255)
  cyan = ident "cyan" >> return (Constant 0, Constant 255, Constant 255)
  magenta = ident "magenta" >> return (Constant 255, Constant 0, Constant 255)
  yellow = ident "yellow" >> return (Constant 255, Constant 255, Constant 0)
  white = ident "white" >> return (Constant 255, Constant 255, Constant 255)

-- |Tokenizes a constant/variable/robot expression.
constant :: Parser Exp
constant = parens constant
         <|> num <|> bool <|> robotline <|> robotultrason <|> var where
  num = fmap Constant number
  var = fmap Variable (some (spot isAlphaNum))
  robotline = ident "linesensor" >> return RobotLineSensor
  robotultrason = ident "ultrason" >> return RobotUltrason

-- |Tokenizes any expression.
expr :: Parser Exp
expr = constant <|> unaryExpr <|> binaryExpr

-- |Tokenizes a number expression.
number :: Parser Double
number = float <|> nat where
  float = do {s <- optional $ string "-"; n <- digits; token '.';
          f <- digits; return $ read (s ++ n ++ "." ++ f)}
  nat = do { s <- optional $ string "-"; n <- digits; return $ read (s ++ n) }

-- |Tokenizes an MBot direction.
robotDirection :: Parser Bot.Direction
robotDirection = parens robotDirection <|> brake <|>forward <|> left <|> right
               <|> backwardleft <|> backwardright<|> backward where
  forward = ident "forward" >> return Bot.DirForward
  left = ident "left" >> return Bot.DirLeft
  right = ident "right" >> return Bot.DirRight
  backward = ident "backward" >> return Bot.DirBackward
  backwardleft = ident "backwardleft" >> return Bot.DirBackwardLeft
  backwardright = ident "backwardright" >> return Bot.DirBackwardRight
  brake = ident "brake" >> return Bot.Brake

-- |Tokenizes an MBot LED identifier.
robotLed :: Parser Bot.Led
robotLed = left <|> right where
  left = ident "left" >> return Bot.LeftLed
  right = ident "right" >> return Bot.RightLed

-- |Tokenizes a unary expression.
unaryExpr :: Parser Exp
unaryExpr = parens unaryExpr <|> absval <|> notval where
  absval = fmap (Unary Abs) (between '|' '|' expr)
  notval = fmap (Unary Not) (token '!' >> expr)
