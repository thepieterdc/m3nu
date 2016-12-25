module Evaluator(module Evaluator, module Environment, module Types) where

import Environment
import Types

evaluate :: Statement -> Environment -> IO Environment
evaluate (Debug s) = evaluateDebug s
evaluate (Order val var) = evaluateOrder val var
evaluate (Puke v) = evaluatePuke v
evaluate Review = return

evaluateArithExp :: ArithExp -> Environment -> IO Double
evaluateArithExp (ArithConst c) _ = return c

evaluateBoolExp :: BoolExp -> Environment -> IO Bool
evaluateBoolExp (BoolConst b) _ = return b
evaluateBoolExp (BoolNegate bExp) e = do { ex <- evaluateBoolExp bExp e; return $ not ex}

evaluateDebug :: String -> Environment -> IO Environment
evaluateDebug txt e = do { print txt; return e}

evaluateOrder :: String -> ArithExp -> Environment -> IO Environment
evaluateOrder var val e = do
  x <- evaluateArithExp val e
  return $ setVariable var x e

evaluatePuke :: ArithExp -> Environment -> IO Environment
evaluatePuke (ArithConst c) e = do { print c; return e}
