module Parser(parseFile, parseString) where

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
statementParser = pukeParser
                -- reviewParser
                -- <|> eatingParser
                -- <|> hungryParser
                -- <|> orderParser
                -- <|> pukeParser
                -- <|> cookParser
                -- <|> robotDriveParser
                -- <|> robotLedParser

-- parses cook (sleep)
cookParser :: Parser Statement
cookParser = do
  _ <- string "cook"
  amt <- expr
  _ <- end
  return $ Cook amt

-- parses eating (while)
eatingParser :: Parser Statement
eatingParser = do
  _ <- string "eating"
  cond <- expr
  _ <- string "->"
  action <- parse
  _ <- keyword "enough"
  return $ Eating cond action

-- parses hungry (if else)
hungryParser :: Parser Statement
hungryParser = do
  _ <- string "hungry"
  cond <- expr
  _ <- string "->"
  ifClause <- parse
  elseClause <- ifelse <|> none
  _ <- keyword "satisfied"
  return $ Hungry cond ifClause elseClause where
    ifelse = do {_ <- string "stuffed"; _ <- string "->"; parse }
    none = return Review

-- parses orders (assignments)
orderParser :: Parser Statement
orderParser = do
  _ <- string "order"
  var <- many (spot isAlphaNum)
  _ <- string "->"
  val <- expr
  _ <- end
  return $ Order var val

-- parses pukes (prints)
pukeParser :: Parser Statement
pukeParser = do
  _ <- string "puke"
  var <- expr
  _ <- end
  return $ Puke var

-- parses reviews (comments -> destroying these)
reviewParser :: Parser Statement
reviewParser = do
  _ <- string "review"
  _ <- skipUntil end
  return Review

robotDriveParser :: Parser Statement
robotDriveParser = do
  _ <- string "drive"
  dir <- robotDirection
  _ <- end
  return $ RobotDrive dir

robotLedParser :: Parser Statement
robotLedParser = do
  _ <- string "led"
  l <- robotLed
  _ <- string "->"
  col <- color
  _ <- end
  return $ RobotLeds l col

parseString :: String -> IO Statement
parseString code = return $ doParse parse $ preprocess code

parseFile :: String -> IO Statement
parseFile file = do { code <- readFile file; return $ doParse parse $ preprocess code }
