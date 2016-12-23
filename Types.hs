module Types where

data BoolExp = BoolConst Bool
             | BoolNegate BoolExp
             | BoolBinary BoolBinaryOp BoolExp BoolExp
             | RelationalBinary RelationalBinaryOp ArithExp ArithExp
             deriving Show
data BoolBinaryOp = And | Or deriving Show

data RelationalBinaryOp = Greater | Less | Equals deriving Show

data ArithExp = Variable String
              | ArithConst Double
              deriving Show

data Statement = Seq [Statement]
               | Order String ArithExp
               | Hungry BoolExp Statement Statement
               | Eating BoolExp Statement
               deriving Show

newtype Parser a = Parser (String -> [(a, String)])
