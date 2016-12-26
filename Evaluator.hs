module Evaluator(module Evaluator, module Environment, module Types) where

import Control.Concurrent(threadDelay)
import Data.Maybe
import Environment
import Types

evaluate :: Statement -> Environment -> IO Environment
evaluate (Cook a) = evaluateCook a
evaluate (Debug s) = evaluateDebug s
evaluate (Eating cond s) = evaluateEating cond s
evaluate (Hungry cond t d) = evaluateHungry cond t d
evaluate (Order val var) = evaluateOrder val var
evaluate (Puke v) = evaluatePuke v
evaluate Review = return
evaluate (Seq s) = evaluateSequence s

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
evaluateBoolExp (BoolBinary op x y) env = evaluateBoolBinaryExp op x y env
evaluateBoolExp (BoolConst b) _ = return b
evaluateBoolExp (BoolNegate bExp) env = do { ex <- evaluateBoolExp bExp env; return $ not ex}
evaluateBoolExp (RelationalBinary op x y) env = evaluateRelationalExp op x y env

evaluateBoolBinaryExp :: BoolBinaryOp -> BoolExp -> BoolExp -> Environment -> IO Bool
evaluateBoolBinaryExp And x y env = do { xe <- evaluateBoolExp x env; ye <- evaluateBoolExp y env; return $ xe && ye}
evaluateBoolBinaryExp Or x y env = do { xe <- evaluateBoolExp x env; ye <- evaluateBoolExp y env; return $ xe || ye}
evaluateBoolBinaryExp BoolEquals x y env = do { xe <- evaluateBoolExp x env; ye <- evaluateBoolExp y env; return $ xe == ye }

evaluateCook :: ArithExp -> Environment -> IO Environment
evaluateCook amtexp env = do { amt <- evaluateArithExp amtexp env; _ <- threadDelay $ round $ amt*1000000; return env}

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

evaluateRelationalExp :: RelationalBinaryOp -> ArithExp -> ArithExp -> Environment -> IO Bool
evaluateRelationalExp Greater x y env = do { xe <- evaluateArithExp x env; ye <- evaluateArithExp y env; return $ xe > ye}
evaluateRelationalExp RelEquals x y env = do { xe <- evaluateArithExp x env; ye <- evaluateArithExp y env; return $ xe == ye}
evaluateRelationalExp Less x y env = do { xe <- evaluateArithExp x env; ye <- evaluateArithExp y env; return $ xe < ye}

evaluateSequence :: [Statement] -> Environment -> IO Environment
evaluateSequence [] env = return env
evaluateSequence (s:sq) env = do
  env' <- evaluate s env
  evaluateSequence sq env'
