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
multipleStatementsParser = do {mult <- many statementParser; return $ Seq mult}

-- parses a statement
statementParser :: Parser Statement
statementParser = reviewParser
                <|> eatingParser
                <|> hungryParser
                <|> orderParser
                <|> pukeParser
                <|> cookParser
                <|> robotDriveParser
                <|> robotLedParser

-- parses cook (sleep)
cookParser :: Parser Statement
cookParser = do {ident "cook"; amt <- expr; end; return $ Cook amt}

-- parses eating (while)
eatingParser :: Parser Statement
eatingParser = do
  ident "eating"
  cond <- expr
  ident "->"
  action <- parse
  keyword "enough"
  return $ Eating cond action

-- parses hungry (if else)
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

-- parses orders (assignments)
orderParser :: Parser Statement
orderParser = do
  ident "order"
  var <- many (spot isAlphaNum)
  ident "->"
  val <- expr
  end
  return $ Order var val

-- parses pukes (prints)
pukeParser :: Parser Statement
pukeParser = do {ident "puke"; var <- expr; end; return $ Puke var}

-- parses reviews (comments -> destroying these)
reviewParser :: Parser Statement
reviewParser = string "review" >> skipUntil end >> return Review

robotDriveParser :: Parser Statement
robotDriveParser = do {ident "drive"; dir <- robotDirection; end;
                   return $ RobotDrive dir}

robotLedParser :: Parser Statement
robotLedParser = do {ident "led"; l <- robotLed; ident "->"; col <- color; end;
                 return $ RobotLeds l col}

parseString :: String -> IO Statement
parseString code = return $ doParse parse $ preprocess code

parseFile :: String -> IO Statement
parseFile file = do { code <- readFile file; return $ doParse parse $ preprocess code }
