module Lexer(module Lexer, module Types, module Data.Char) where

import Data.Char
--
import qualified MBotPlus as Bot
import Types

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

-- parses the end of a line
endline :: Parser ()
endline = do { _ <- some semicolon; return ()}

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

-- matches a given predicate
spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c}

-- matches a string
string :: String -> Parser String
string = mapM token

-- matches a given char
token :: Char -> Parser Char
token c = spot (== c)

-- matches an uppercase letter
upper :: Parser Char
upper = spot isUpper

-- preproceses a file for parsing, removes all whitespace
preprocess :: String -> String
preprocess = filter (`notElem` [' ', '\t', '\n', '\r'])

-- [ TOKENIZERS ] --

-- parses a binary expression
tokenizeBinExp :: Parser Exp
tokenizeBinExp = add <|> sub <|> mul <|> dvd <|> binand <|> binor where
  add = parens $ bin "+" Add;
  sub = parens $ bin "-" Minus;
  mul = parens $ bin "*" Multiply;
  dvd = parens $ bin "/" Divide;
  binand = parens $ bin "and" And;
  binor = parens $ bin "or" Or;
  bin tk op = do { x <- tokenizeExp; _ <- string tk; y <- tokenizeExp; return $ Binary op x y}

-- parses a boolean
tokenizeBool :: Parser Exp
tokenizeBool = true <|> false where
  true = do { _ <- string "tasty"; return $ Constant 1 }
  false = do { _ <- string "disguisting"; return $ Constant 0 }

tokenizeColor :: Parser Color
tokenizeColor = rgb <|> off <|> white <|> red <|> green <|> blue
                <|> cyan <|> yellow <|> magenta where
  rgb = do { r <- tokenizeExp; g <- tokenizeExp; b <- tokenizeExp; return (r, g, b)}
  off = do { _ <- string "off"; return (Constant 0, Constant 0, Constant 0)}
  red = do { _ <- string "red"; return (Constant 255, Constant 0, Constant 0)}
  green = do { _ <- string "green"; return (Constant 0, Constant 255, Constant 0)}
  blue = do { _ <- string "blue"; return (Constant 0, Constant 0, Constant 255)}
  cyan = do { _ <- string "cyan"; return (Constant 0, Constant 255, Constant 255)}
  magenta = do { _ <- string "magenta"; return (Constant 255, Constant 0, Constant 255)}
  yellow = do { _ <- string "yellow"; return (Constant 255, Constant 255, Constant 0)}
  white = do { _ <- string "white"; return (Constant 255, Constant 255, Constant 255)}

-- parses an expr
tokenizeExp :: Parser Exp
tokenizeExp = parens tokenizeBool <|> tokenizeBool
            <|> parens robotline <|> robotline
            <|> parens robotultrason <|> robotultrason
            <|> parens num <|> num <|> parens var <|> var
            <|> tokenizeBinExp <|> tokenizeUnaryExp <|> tokenizeRelExp where
  num = do { n <- tokenizeNumber; return $ Constant n }
  var = do { x <- some (spot isAlphaNum); return $ Variable x }
  robotline = do { _ <- string "linesensor"; return RobotLineSensor}
  robotultrason = do { _ <- string "ultrason"; return RobotUltrason}

-- parses a double number
tokenizeNumber :: Parser Double
tokenizeNumber = float <|> negFloat <|> nat <|> negNat where
  float = do { n <- digits; dot <- token '.'; f <- digits; return $ read (n ++ [dot] ++ f)}
  nat = do { s <- digits; return $ read s}
  negFloat = do { _ <- token '-'; n <- float; return $ -n}
  negNat = do { _ <- token '-'; n <- nat; return $ -n}

tokenizeRelExp :: Parser Exp
tokenizeRelExp = gteq <|> lteq <|> gt <|> lt <|> eq where
  gteq = parens $ rel ">=" GrEquals;
  lteq = parens $ rel "<=" LtEquals;
  gt = parens $ rel ">" Greater;
  lt = parens $ rel "<" Less;
  eq = parens $ rel "==" Equals;
  rel tk op = do { x <- tokenizeExp; _ <- string tk; y <- tokenizeExp; return $ Relational op x y}

tokenizeRobotDirection :: Parser Bot.Direction
tokenizeRobotDirection = parens tokenizeRobotDirection <|> brake <|>forward
                       <|> left <|> right
                       <|> backwardleft <|> backwardright<|> backward where
  forward = do { _ <- string "forward"; return Bot.DirForward}
  left = do { _ <- string "left"; return Bot.DirLeft}
  right = do { _ <- string "right"; return Bot.DirRight}
  backward = do { _ <- string "backward"; return Bot.DirBackward}
  backwardleft = do { _ <- string "backwardleft"; return Bot.DirBackwardLeft}
  backwardright = do { _ <- string "backwardright"; return Bot.DirBackwardRight}
  brake = do { _ <- string "brake"; return Bot.Brake}

tokenizeRobotLed :: Parser Bot.Led
tokenizeRobotLed = left <|> right where
  left = do { _ <- identifier "left"; return Bot.LeftLed}
  right = do { _ <- identifier "right"; return Bot.RightLed}

-- parses a Unary expression
tokenizeUnaryExp :: Parser Exp
tokenizeUnaryExp = parens absval <|> absval
                 <|> parens notval <|> notval where
  absval = do { ret <- between '|' '|' tokenizeExp; return $ Unary Abs ret }
  notval = do { _ <- identifier "not"; ret <- tokenizeExp; return $ Unary Not ret }

-- skips until a given token, returning the skipped part including the cond obv because parsed
tokenizeUntil :: Parser a -> Parser String
tokenizeUntil cond = done <|> oncemore where
  done = do { _ <- cond; return ""}
  oncemore = do { c <- char; b <- tokenizeUntil cond; return $ c : b}
