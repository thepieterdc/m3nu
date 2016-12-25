module Evaluator(module Evaluator, module Environment, module Types) where

import Data.Maybe
import Environment
import Types

evaluate :: Statement -> Environment -> IO Environment
evaluate (Debug s) = evaluateDebug s
evaluate (Order val var) = evaluateOrder val var
evaluate (Puke v) = evaluatePuke v
evaluate Review = return

evaluateArithExp :: ArithExp -> Environment -> IO Double
evaluateArithExp (ArithConst c) _ = return c
evaluateArithExp (Variable v) env = return $ fromMaybe (error $ "Unknown order: " ++ v) (getVariable v env)

evaluateBoolExp :: BoolExp -> Environment -> IO Bool
evaluateBoolExp (BoolConst b) _ = return b
evaluateBoolExp (BoolNegate bExp) env = do { ex <- evaluateBoolExp bExp env; return $ not ex}

evaluateDebug :: String -> Environment -> IO Environment
evaluateDebug txt env = do { print txt; return env}

evaluateOrder :: String -> ArithExp -> Environment -> IO Environment
evaluateOrder var val env = do
  x <- evaluateArithExp val env
  return $ setVariable var x env

evaluatePuke :: ArithExp -> Environment -> IO Environment
evaluatePuke e env = do { val <- evaluateArithExp e env; print val; return env}
