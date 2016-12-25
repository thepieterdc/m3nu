module Parser where

import Lexer
import Types

parse :: Parser Statement
parse = reviewParser
        <|> orderParser
        <|> pukeParser
        <|> debugParser

-- parses anything for debugging
debugParser :: Parser Statement
debugParser = do { o <- many (spot isAscii); return $ Debug o}

-- parses orders (assignments)
orderParser :: Parser Statement
orderParser = do
  _ <- identifier "order"
  var <- many (spot isAlphaNum)
  _ <- spaces
  val <- tokenizeNumber
  _ <- endline
  return $ Order var (ArithConst val)

-- parses pukes (prints)
pukeParser :: Parser Statement
pukeParser = do
  _ <- identifier "puke"
  var <- tokenizeArithExp
  _ <- endline
  return $ Puke var

-- parses reviews (comments -> destroying these)
reviewParser :: Parser Statement
reviewParser = do
  _ <- identifier "review"
  _ <- tokenizeUntil endline
  _ <- endline
  return Review

parseString :: String -> Statement
parseString = doParse parse

parseFile :: String -> IO Statement
parseFile file = do { code <- readFile file; return $ doParse parse code }
