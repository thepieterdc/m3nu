module Evaluator(module Evaluator, module Types) where

import Control.Concurrent(threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import MBot

import Types

evaluate :: Statement -> Environment ()
evaluate (Cook a) = evaluateCook a
evaluate (Debug s) = evaluateDebug s
evaluate (Eating cond s) = evaluateEating cond s
evaluate (Hungry cond t d) = evaluateHungry cond t d
evaluate (Order val var) = evaluateOrder val var
evaluate (Puke v) = evaluatePuke v
evaluate Review = return ()
evaluate (RobotDrive d) = evaluateRobotDrive d
evaluate (RobotLeds l r g b) = evaluateRobotLed l r g b
evaluate (Seq s) = evaluateSequence s

evaluateBinaryExp :: BinaryOp -> Exp -> Exp -> Environment Double
evaluateBinaryExp Add x y = do { xe <- evaluateExp x; ye <- evaluateExp y; return $ xe+ye}
evaluateBinaryExp Minus x y = do { xe <- evaluateExp x; ye <- evaluateExp y; return $ xe-ye}
evaluateBinaryExp Multiply x y = do { xe <- evaluateExp x; ye <- evaluateExp y; return $ xe*ye}
evaluateBinaryExp Divide x y = do { xe <- evaluateExp x; ye <- evaluateExp y; return $ xe/ye}
evaluateBinaryExp And x y = do { xe <- evaluateExp x; ye <- evaluateExp y; return $ boolDouble $ xe /= 0 && ye /= 0}
evaluateBinaryExp Or x y = do { xe <- evaluateExp x; ye <- evaluateExp y; return $ boolDouble $ xe /= 0 || ye /= 0}

evaluateCook :: Exp -> Environment ()
evaluateCook amtexp = do { amt <- evaluateExp amtexp; liftIO $ threadDelay $ round $ amt*1000000; return ()}

evaluateDebug :: String -> Environment ()
evaluateDebug txt = do { liftIO $ print txt; return ()}

evaluateEating :: Exp -> Statement -> Environment ()
evaluateEating cond task = do
  loopcond <- evaluateExp cond
  when (doubleBool loopcond) $ do {_ <- evaluate task; evaluateEating cond task}

evaluateExp :: Exp -> Environment Double
evaluateExp (Constant c) = return c
evaluateExp (Variable v) = environmentGet v
evaluateExp (Binary op x y) = evaluateBinaryExp op x y
evaluateExp (Unary op x) = evaluateUnaryExp op x
evaluateExp (Relational op x y) = evaluateRelationalExp op x y
evaluateExp RobotLineSensor = evaluateRobotLineSensor

evaluateHungry :: Exp -> Statement -> Statement -> Environment ()
evaluateHungry cond ifc elsec = do
  c <- evaluateExp cond
  if doubleBool c then evaluate ifc else evaluate elsec

evaluateOrder :: String -> Exp -> Environment ()
evaluateOrder var v = do { x <- evaluateExp v; environmentSet var x; return () }

evaluatePuke :: Exp -> Environment ()
evaluatePuke e = do { val <- evaluateExp e; liftIO $ print val; return ()}

evaluateRelationalExp :: RelationalOp -> Exp -> Exp -> Environment Double
evaluateRelationalExp Greater x y = do { xe <- evaluateExp x; ye <- evaluateExp y; return $ boolDouble $ xe > ye}
evaluateRelationalExp Equals x y = do { xe <- evaluateExp x; ye <- evaluateExp y; return $ boolDouble $ xe == ye}
evaluateRelationalExp Less x y = do { xe <- evaluateExp x; ye <- evaluateExp y; return $ boolDouble $ xe < ye}

evaluateRobotDrive :: RobotDirection -> Environment ()
evaluateRobotDrive dir = do
  handle <- liftIO openMBot
  liftIO $ robotMotorDirection dir handle
  liftIO $ closeMBot handle

evaluateRobotLed :: RobotLed -> Exp -> Exp -> Exp -> Environment ()
evaluateRobotLed l r g b = do
  rv <- evaluateExp r
  gv <- evaluateExp g
  bv <- evaluateExp b
  handle <- liftIO openMBot
  liftIO $ sendCommand handle $ setRGB (robotLedId l) (doubleInt rv) (doubleInt gv) (doubleInt bv)
  liftIO $ closeMBot handle

evaluateRobotLineSensor :: Environment Double
evaluateRobotLineSensor = do
  handle <- liftIO openMBot
  val <- liftIO $ readLineFollower handle
  liftIO $ closeMBot handle
  return $ robotLineDouble val

evaluateSequence :: [Statement] -> Environment ()
evaluateSequence [] = return ()
evaluateSequence (s:sq) = do {_ <- evaluate s; evaluateSequence sq}

evaluateUnaryExp :: UnaryOp -> Exp -> Environment Double
evaluateUnaryExp Abs x = do { e <- evaluateExp x; return $ if e < 0 then -e else e }
evaluateUnaryExp Not x = do { e <- evaluateExp x; return $ if e == 0 then 1 else 0}
