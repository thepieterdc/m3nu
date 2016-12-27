module Parser where

import Lexer
import Types

parse :: Parser Statement
parse = parens parse <|> multipleStatementsParser

doParse :: (Show a) => Parser a -> String -> a
doParse m s = one [x | (x,t) <- apply m s, t == "" ] where
  one [x] = x
  one [] = error "Parse not completed."
  one xs | length xs > 1 = error ("Multiple parses found:\n " ++ show xs)
  one _ = error "Unknown parse error."

-- parses multiple statements
multipleStatementsParser :: Parser Statement
multipleStatementsParser = do { mult <- many statementParser; return $ Seq mult}

-- parses a statement
statementParser :: Parser Statement
statementParser = reviewParser
                <|> eatingParser
                <|> hungryParser
                <|> orderParser
                <|> pukeParser
                <|> cookParser
                -- <|> debugParser -- vervangen door error

-- parses cook (sleep)
cookParser :: Parser Statement
cookParser = do
  _ <- identifier "cook"
  amt <- tokenizeExp
  _ <- endline
  return $ Cook amt

-- parses anything for debugging
debugParser :: Parser Statement
debugParser = do { o <- many (spot isAscii); return $ Debug o}

-- parses eating (while)
eatingParser :: Parser Statement
eatingParser = do
  _ <- identifier "eating"
  cond <- tokenizeExp
  _ <- identifier "{"
  action <- parse
  _ <- identifier "}"
  return $ Eating cond action

-- parses hungry (if else)
hungryParser :: Parser Statement
hungryParser = do
  _ <- identifier "hungry"
  cond <- tokenizeExp
  _ <- identifier "{"
  ifClause <- parse
  _ <- whitespace
  _ <- identifier "}"
  _ <- identifier "stuffed"
  _ <- identifier "{"
  elseClause <- parse
  _ <- whitespace
  _ <- identifier "}"
  return $ Hungry cond ifClause elseClause

-- parses orders (assignments)
orderParser :: Parser Statement
orderParser = do
  _ <- identifier "order"
  var <- many (spot isAlphaNum)
  _ <- spaces
  val <- tokenizeExp
  _ <- endline
  return $ Order var val

-- parses pukes (prints)
pukeParser :: Parser Statement
pukeParser = do
  _ <- whitespace
  _ <- identifier "puke"
  var <- tokenizeExp
  _ <- endline
  return $ Puke var

-- parses reviews (comments -> destroying these)
reviewParser :: Parser Statement
reviewParser = do
  _ <- identifier "review"
  _ <- tokenizeUntil endline
  return Review

parseString :: String -> IO Statement
parseString code = return $ doParse parse $ trim code

parseFile :: String -> IO Statement
parseFile file = do { code <- readFile file; return $ doParse parse $ trim code }
