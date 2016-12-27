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

evaluateExp :: Exp -> Environment -> IO Double
evaluateExp (Constant c) _ = return c
evaluateExp (Variable v) env = return $ fromMaybe (error $ "Unknown variable: " ++ v) (getVariable v env)
evaluateExp (Binary op x y) env = evaluateBinaryExp op x y env
evaluateExp (Unary op x) env = evaluateUnaryExp op x env
evaluateExp (Relational op x y) env = evaluateRelationalExp op x y env

evaluateBinaryExp :: BinaryOp -> Exp -> Exp -> Environment -> IO Double
evaluateBinaryExp Add x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ xe+ye}
evaluateBinaryExp Minus x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ xe-ye}
evaluateBinaryExp Multiply x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ xe*ye}
evaluateBinaryExp Divide x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ xe/ye}
evaluateBinaryExp And x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe /= 0 && ye /= 0}
evaluateBinaryExp Or x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe /= 0 || ye /= 0}

evaluateCook :: Exp -> Environment -> IO Environment
evaluateCook amtexp env = do { amt <- evaluateExp amtexp env; _ <- threadDelay $ round $ amt*1000000; return env}

evaluateDebug :: String -> Environment -> IO Environment
evaluateDebug txt env = do { print txt; return env}

evaluateEating :: Exp -> Statement -> Environment -> IO Environment
evaluateEating cond task env = do
  loopcond <- evaluateExp cond env
  if doubleBool loopcond then do
    env' <- evaluate task env
    evaluateEating cond task env'
  else return env

evaluateHungry :: Exp -> Statement -> Statement -> Environment -> IO Environment
evaluateHungry cond ifc elsec env = do
  c <- evaluateExp cond env
  if doubleBool c then evaluate ifc env else evaluate elsec env

evaluateOrder :: String -> Exp -> Environment -> IO Environment
evaluateOrder var val env = do
  x <- evaluateExp val env
  return $ setVariable var x env

evaluatePuke :: Exp -> Environment -> IO Environment
evaluatePuke e env = do { val <- evaluateExp e env; print val; return env}

evaluateRelationalExp :: RelationalOp -> Exp -> Exp -> Environment -> IO Double
evaluateRelationalExp Greater x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe > ye}
evaluateRelationalExp Equals x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe == ye}
evaluateRelationalExp Less x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe < ye}

evaluateSequence :: [Statement] -> Environment -> IO Environment
evaluateSequence [] env = return env
evaluateSequence (s:sq) env = do
  env' <- evaluate s env
  evaluateSequence sq env'

evaluateUnaryExp :: UnaryOp -> Exp -> Environment -> IO Double
evaluateUnaryExp Abs x env = do { e <- evaluateExp x env; return $ if x < 0 then -e else e }
