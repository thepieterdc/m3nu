module Evaluator(module Evaluator, module Environment, module Types) where

import Data.Maybe
import Environment
import Types

evaluate :: Statement -> Environment -> IO Environment
evaluate (Debug s) = evaluateDebug s
evaluate (Eating cond s) = evaluateEating cond s
evaluate (Hungry cond t d) = evaluateHungry cond t d
evaluate (Order val var) = evaluateOrder val var
evaluate (Puke v) = evaluatePuke v
evaluate Review = return

evaluateArithExp :: ArithExp -> Environment -> IO Double
evaluateArithExp (ArithConst c) _ = return c
evaluateArithExp (ArithBinary op x y) env = evaluateArithBinaryExp op x y env
evaluateArithExp (Variable v) env = return $ fromMaybe (error $ "Unknown variable: " ++ v) (getVariable v env)

evaluateArithBinaryExp :: ArithBinaryOp -> ArithExp -> ArithExp -> Environment -> IO Double
evaluateArithBinaryExp Add x y env = do { xe <- evaluateArithExp x env; ye <- evaluateArithExp y env; return $ xe+ye}
evaluateArithBinaryExp Minus x y env = do { xe <- evaluateArithExp x env; ye <- evaluateArithExp y env; return $ xe-ye}
evaluateArithBinaryExp Multiply x y env = do { xe <- evaluateArithExp x env; ye <- evaluateArithExp y env; return $ xe*ye}
evaluateArithBinaryExp Divide x y env = do { xe <- evaluateArithExp x env; ye <- evaluateArithExp y env; return $ xe/ye}

evaluateBoolExp :: BoolExp -> Environment -> IO Bool
evaluateBoolExp (BoolConst b) _ = return b
evaluateBoolExp (BoolNegate bExp) env = do { ex <- evaluateBoolExp bExp env; return $ not ex}

evaluateDebug :: String -> Environment -> IO Environment
evaluateDebug txt env = do { print txt; return env}

evaluateEating :: BoolExp -> Statement -> Environment -> IO Environment
evaluateEating cond task env = do
  loopcond <- evaluateBoolExp cond env
  if loopcond then do
    env' <- evaluate task env
    evaluateEating cond task env'
  else return env

evaluateHungry :: BoolExp -> Statement -> Statement -> Environment -> IO Environment
evaluateHungry cond ifc elsec env = do
  c <- evaluateBoolExp cond env
  if c then evaluate ifc env
  else evaluate elsec env

evaluateOrder :: String -> ArithExp -> Environment -> IO Environment
evaluateOrder var val env = do
  x <- evaluateArithExp val env
  return $ setVariable var x env

evaluatePuke :: ArithExp -> Environment -> IO Environment
evaluatePuke e env = do { val <- evaluateArithExp e env; print val; return env}
