module Parser where

import Lexer
import Types

parse :: Parser Statement
parse = reviewParser
        <|> pukeParser
        <|> anyParser

anyParser :: Parser Statement
anyParser = do { o <- many (spot isAscii); return (Puke o)}

-- parses pukes (prints)
pukeParser :: Parser Statement
pukeParser = do
  _ <- identifier "puke"
  txt <- tokenizeString
  _ <- endline
  return $ Puke txt

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
parseFile file = do { code <- readFile file; return (doParse parse code) }
