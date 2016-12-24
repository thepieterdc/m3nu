module Parser where

import Lexer
import Types

parse :: Parser Statement
parse = --reviewParser
        pukeParser
        <|> anyParser

anyParser :: Parser Statement
anyParser = do { o <- many (spot isAscii); return (Puke o)}

pukeParser :: Parser Statement
pukeParser = do
  _ <- identifier "puke"
  txt <- tokenizeString
  _ <- endline
  return $ Puke txt

-- reviewParser :: Parser Statement
-- reviewParser = do
--   _ <- string "review"

parseString :: String -> Statement
parseString = doParse parse

parseFile :: String -> IO Statement
parseFile file = do { code <- readFile file; return (doParse parse code) }
