module Parser where

import Lexer
import Types

parse :: Parser Statement
parse = reviewParser
        <|> eatingParser
        <|> hungryParser
        <|> orderParser
        <|> pukeParser
        <|> debugParser

-- parses anything for debugging
debugParser :: Parser Statement
debugParser = do { o <- many (spot isAscii); return $ Debug o}

-- parses eating (while)
eatingParser :: Parser Statement
eatingParser = do
  _ <- identifier "eating"
  cond <- tokenizeBoolExp
  _ <- identifier "{"
  action <- parse
  _ <- identifier "}"
  return $ Eating cond action

-- parses hungry (if else)
hungryParser :: Parser Statement
hungryParser = do
  _ <- identifier "hungry"
  cond <- tokenizeBoolExp
  _ <- identifier "{"
  ifClause <- parse
  _ <- identifier "}"
  _ <- identifier "stuffed"
  _ <- identifier "{"
  elseClause <- parse
  _ <- identifier "}"
  return $ Hungry cond ifClause elseClause

-- parses orders (assignments)
orderParser :: Parser Statement
orderParser = do
  _ <- identifier "order"
  var <- many (spot isAlphaNum)
  _ <- spaces
  val <- tokenizeArithExp
  -- _ <- endline
  rest <- debugParser
  -- return $ Order var val
  return $ rest

-- parses pukes (prints)
pukeParser :: Parser Statement
pukeParser = do
  _ <- whitespace
  _ <- identifier "puke"
  var <- tokenizeArithExp
  _ <- endline
  return $ Puke var

-- parses reviews (comments -> destroying these)
reviewParser :: Parser Statement
reviewParser = do
  _ <- identifier "review"
  _ <- tokenizeUntil endline
  return Review

parseString :: String -> Statement
parseString = doParse parse

parseFile :: String -> IO Statement
parseFile file = do { code <- readFile file; return $ doParse parse code }
