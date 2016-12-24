module Parser where

import Types

parse :: Parser Statement
parse = return $ Puke "Test"

parseString :: String -> Statement
parseString = doParse parse

parseFile :: String -> IO Statement
parseFile file = do { code <- readFile file; return (doParse parse code) }
