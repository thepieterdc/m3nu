module Lexer(module Lexer, module Data.Char) where

import Data.Char

import qualified MBotPlus as Bot
import Types hiding(optional)

-- parses between two delims
between :: Char -> Char -> Parser a -> Parser a
between l r p = do { _ <- token l; ret <- p; _ <- token r; return ret}

-- runs the parser between { }
brackets :: Parser a -> Parser a
brackets = between '{' '}'

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

-- matches the end of an instruction
end :: Parser ()
end = do { _ <- some semicolon; return ()}

-- parses an identifier
ident :: String -> Parser ()
ident k = do {_ <- string k; return ()}

-- parses a keyword, must be followed by a semicolon
keyword :: String -> Parser ()
keyword k = do { _ <- string k; _ <- end; return ()}

-- parsers a letter
letter :: Parser Char
letter = spot isAlpha

-- parses multiple letters
letters :: Parser String
letters = some letter

-- parses a lowercase letter
lower :: Parser Char
lower = spot isLower

-- parses an optional string
optional :: Parser String -> Parser String
optional a = a <|> return []

-- runs the parser between ( )
parens :: Parser a -> Parser a
parens = between '(' ')'

-- parses a semicolon
semicolon :: Parser ()
semicolon = token ';'

-- skips until a given token, returning the skipped part including the cond
-- obv because parsed
skipUntil :: Parser a -> Parser String
skipUntil cond = done <|> oncemore where
  done = do { _ <- cond; return ""}
  oncemore = do { c <- char; b <- skipUntil cond; return $ c : b}

-- matches a given predicate
spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c}

-- matches a string
string :: String -> Parser String
string = mapM (\x -> spot (== x))

-- matches a given char
token :: Char -> Parser ()
token c = void $ spot (== c)

-- matches an uppercase letter
upper :: Parser Char
upper = spot isUpper

-- preproceses a file for parsing, removes all whitespace
preprocess :: String -> String
preprocess = filter (`notElem` [' ', '\t', '\n', '\r'])

-- [ TOKENIZERS ] --

bool :: Parser Exp
bool = true <|> false where
  true = do { ident "tasty"; return $ Constant 1 }
  false = do { ident "disguisting"; return $ Constant 0 }

binaryExpr :: Parser Exp
binaryExpr = add <|> sub <|> mul <|> dvd
           <|> booland <|> boolor <|> gteq <|> lteq <|> gt <|> lt <|> eq where
  binpart = constant <|> unaryExpr <|> binaryExpr
  bin tk = do { x <- binpart; _ <- string tk; y <- binpart; return (x,y)}
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

color :: Parser Color
color = rgb <|> off <|> white <|> red <|> green <|> blue
                <|> cyan <|> yellow <|> magenta where
  rgbpart = do{ret <- expr; token ','; return ret}
  rgb = do {r <- rgbpart; g <- rgbpart; b <- expr; return (r, g, b)}
  off = do {ident "off"; return (Constant 0, Constant 0, Constant 0)}
  red = do {ident "red"; return (Constant 255, Constant 0, Constant 0)}
  green = do {ident "green"; return (Constant 0, Constant 255, Constant 0)}
  blue = do {ident "blue"; return (Constant 0, Constant 0, Constant 255)}
  cyan = do {ident "cyan"; return (Constant 0, Constant 255, Constant 255)}
  magenta = do {ident "magenta"; return (Constant 255, Constant 0, Constant 255)}
  yellow = do {ident "yellow"; return (Constant 255, Constant 255, Constant 0)}
  white = do {ident "white"; return (Constant 255, Constant 255, Constant 255)}

-- constant expression
constant :: Parser Exp
constant = parens constant
         <|> num <|> bool <|> robotline <|> robotultrason <|> var where
  num = do { n <- number; return $ Constant n }
  var = do { x <- some (spot isAlphaNum); return $ Variable x }
  robotline = ident "linesensor" >> return RobotLineSensor
  robotultrason = ident "ultrason" >> return RobotUltrason

-- parses an expr
expr :: Parser Exp
expr = constant <|> unaryExpr <|> binaryExpr

-- parses a double number
number :: Parser Double
number = float <|> nat where
  float = do { s <- optional $ string "-"; n <- digits; token '.';
          f <- digits; return $ read (s ++ n ++ "." ++ f)}
  nat = do { s <- optional $ string "-"; n <- digits; return $ read (s ++ n) }

robotDirection :: Parser Bot.Direction
robotDirection = parens robotDirection <|> brake <|>forward <|> left <|> right
               <|> backwardleft <|> backwardright<|> backward where
  forward = do { ident "forward"; return Bot.DirForward}
  left = do { ident "left"; return Bot.DirLeft}
  right = do { ident "right"; return Bot.DirRight}
  backward = do { ident "backward"; return Bot.DirBackward}
  backwardleft = do { ident "backwardleft"; return Bot.DirBackwardLeft}
  backwardright = do { ident "backwardright"; return Bot.DirBackwardRight}
  brake = do { ident "brake"; return Bot.Brake}

robotLed :: Parser Bot.Led
robotLed = left <|> right where
  left = do { ident "left"; return Bot.LeftLed}
  right = do { ident "right"; return Bot.RightLed}

unaryExpr :: Parser Exp
unaryExpr = parens unaryExpr <|> absval <|> notval where
  absval = do { ret <- between '|' '|' expr; return $ Unary Abs ret }
  notval = do { token '!'; ret <- expr; return $ Unary Not ret }
