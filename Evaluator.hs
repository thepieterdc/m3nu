module Evaluator(module Evaluator, module Types) where

import Control.Concurrent(threadDelay)
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Types

evaluate :: Statement -> Environment ()
-- evaluate (Cook a) = evaluateCook a
evaluate (Debug s) = evaluateDebug s
-- evaluate (Eating cond s) = evaluateEating cond s
-- evaluate (Hungry cond t d) = evaluateHungry cond t d
evaluate (Order val var) = evaluateOrder val var
evaluate (Puke v) = evaluatePuke v
evaluate Review = return ()
evaluate (Seq s) = evaluateSequence s

evaluateExp :: Exp -> Environment Double
evaluateExp (Constant c) = return c
evaluateExp (Variable v) = environmentGet v
-- evaluateExp (Binary op x y) env = evaluateBinaryExp op x y env
evaluateExp (Unary op x) = evaluateUnaryExp op x
-- evaluateExp (Relational op x y) env = evaluateRelationalExp op x y env
--
-- evaluateBinaryExp :: BinaryOp -> Exp -> Exp -> Environment Double
-- evaluateBinaryExp Add x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ xe+ye}
-- evaluateBinaryExp Minus x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ xe-ye}
-- evaluateBinaryExp Multiply x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ xe*ye}
-- evaluateBinaryExp Divide x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ xe/ye}
-- evaluateBinaryExp And x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe /= 0 && ye /= 0}
-- evaluateBinaryExp Or x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe /= 0 || ye /= 0}
--
-- evaluateCook :: Exp -> Environment ()
-- evaluateCook amtexp env = do { amt <- evaluateExp amtexp env; _ <- threadDelay $ round $ amt*1000000; return env}

evaluateDebug :: String -> Environment ()
evaluateDebug txt = do { liftIO $ print txt; return ()}

-- evaluateEating :: Exp -> Statement -> Environment ()
-- evaluateEating cond task env = do
--   loopcond <- evaluateExp cond env
--   if doubleBool loopcond then do
--     env' <- evaluate task env
--     evaluateEating cond task env'
--   else return env
--
-- evaluateHungry :: Exp -> Statement -> Statement -> Environment ()
-- evaluateHungry cond ifc elsec env = do
--   c <- evaluateExp cond env
--   if doubleBool c then evaluate ifc env else evaluate elsec env

evaluateOrder :: String -> Exp -> Environment ()
evaluateOrder var v = do { x <- evaluateExp v; environmentSet var x; return () }

evaluatePuke :: Exp -> Environment ()
evaluatePuke e = do { val <- evaluateExp e; liftIO $ print val; return ()}
--
-- evaluateRelationalExp :: RelationalOp -> Exp -> Exp -> Environment Double
-- evaluateRelationalExp Greater x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe > ye}
-- evaluateRelationalExp Equals x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe == ye}
-- evaluateRelationalExp Less x y env = do { xe <- evaluateExp x env; ye <- evaluateExp y env; return $ boolDouble $ xe < ye}

evaluateSequence :: [Statement] -> Environment ()
evaluateSequence [] = return ()
evaluateSequence (s:sq) = do {_ <- evaluate s; evaluateSequence sq}

evaluateUnaryExp :: UnaryOp -> Exp -> Environment Double
evaluateUnaryExp Abs x = do { e <- evaluateExp x; return $ if e < 0 then -e else e }
