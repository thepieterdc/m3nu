{-|
Module      : Parser
Description : Parses code into an Abstract Syntax Tree.
Copyright   : (c) Pieter De Clercq, 2016
License     : MIT
Maintainer  : piedcler.declercq@ugent.be
-}
module Parser(parseFile, parseString) where

import Lexer
import Types

-- |Parses a statement
parse :: Parser Statement
parse = parens parse <|> multipleStatementsParser

{-|
  Runs a parser and creates an Abstract Syntax Tree.
  The entire input string must be consumed.
-}
doParse :: (Show a) => Parser a -> String -> a
doParse m s = one [x | (x,t) <- apply m s, t == "" ] where
  one [x] = x
  one [] = error "Parse not completed."
  one xs | length xs > 1 = error ("Multiple parses found:\n " ++ show xs)
  one _ = error "Unknown parse error."

-- |Parses multiple statements.
multipleStatementsParser :: Parser Statement
multipleStatementsParser = do {mult <- many statementParser; return $ Seq mult}

-- |Parses a statement.
statementParser :: Parser Statement
statementParser = reviewParser
                <|> eatingParser
                <|> hungryParser
                <|> orderParser
                <|> pukeParser
                <|> cookParser
                <|> robotDriveParser
                <|> robotLedParser

-- |Parses a sleep statement.
cookParser :: Parser Statement
cookParser = do {ident "cook"; amt <- expr; end; return $ Cook amt}

-- |Parses a while loop statement.
eatingParser :: Parser Statement
eatingParser = do
  ident "eating"
  cond <- expr
  ident "->"
  action <- parse
  keyword "enough"
  return $ Eating cond action

-- |Parses a if/else conditional statement.
hungryParser :: Parser Statement
hungryParser = do
  ident "hungry"
  cond <- expr
  ident "->"
  ifClause <- parse
  elseClause <- ifelse <|> none
  keyword "satisfied"
  return $ Hungry cond ifClause elseClause where
    ifelse = do {ident "stuffed"; ident "->"; parse }
    none = return Review

-- |Parses an assignment statement.
orderParser :: Parser Statement
orderParser = do
  ident "order"
  var <- many (spot isAlphaNum)
  ident "->"
  val <- expr
  end
  return $ Order var val

-- |Parses a print statement.
pukeParser :: Parser Statement
pukeParser = do {ident "puke"; var <- expr; end; return $ Puke var}

-- |Parses comments (ignoring them).
reviewParser :: Parser Statement
reviewParser = string "review" >> skipUntil end >> return Review

-- |Parses an MBot motor command.
robotDriveParser :: Parser Statement
robotDriveParser = do {ident "drivethrough"; dir <- robotDirection; end;
                   return $ RobotDrive dir}

-- |Parses an MBot LED command.
robotLedParser :: Parser Statement
robotLedParser = do {ident "led"; l <- robotLed; ident "->"; col <- color; end;
                 return $ RobotLeds l col}

-- |Builds an Abstract Syntax Tree from a String.
parseString :: String -> IO Statement
parseString code = return $ doParse parse $ preprocess code

-- |Builds an Abstract Syntax Tree from a file.
parseFile :: String -> IO Statement
parseFile file = do {code <- readFile file;
                 return $ doParse parse $ preprocess code }
