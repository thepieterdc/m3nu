module Parser where

import Lexer
import Types

parse :: Parser Statement
parse = pukeParser
        <|> anyParser

anyParser :: Parser Statement
anyParser = do { o <- many (spot isAscii); return (Puke o)}

pukeParser :: Parser Statement
pukeParser = do
  _ <- string "puke"
  txt <- tokenizeString
  _ <- semicolon
  return $ Puke txt

parseString :: String -> Statement
parseString = doParse parse

parseFile :: String -> IO Statement
parseFile file = do { code <- readFile file; return (doParse parse code) }
