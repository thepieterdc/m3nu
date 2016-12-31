{-|
Module      : Evaluator
Description : Evaluates parsed expressions.
Copyright   : (c) Pieter De Clercq, 2016
License     : MIT
Maintainer  : piedcler.declercq@ugent.be
-}
module Evaluator(eval) where

import Control.Concurrent(threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import qualified MBotPlus as Bot

import Types
import Utils

eval :: Statement -> Environment ()
eval (Cook a) = cook a
eval (Eating cond s) = eating cond s
eval (Hungry cond ifc elsec) = hungry cond ifc elsec
eval (Order val var) = order val var
eval (Puke v) = puke v
eval Review = return ()
eval (RobotDrive d) = robotDrive d
eval (RobotLeds l c) = robotLed l c
eval (Seq s) = sq s

binExpr :: BinaryOp -> Exp -> Exp -> Environment Double
binExpr Add x y = (+) <$> expr x <*> expr y;
binExpr Minus x y = (-) <$> expr x <*> expr y;
binExpr Multiply x y = (*) <$> expr x <*> expr y;
binExpr Divide x y = (/) <$> expr x <*> expr y;

boolExpr :: BooleanOp -> Exp -> Exp -> Environment Double
boolExpr And x y = do {xe <- expr x; ye <- expr y;
                   return $ boolDouble $ xe /= 0 && ye /= 0}
boolExpr Or x y = do {xe <- expr x; ye <- expr y;
                  return $ boolDouble $ xe /= 0 || ye /= 0}

cook :: Exp -> Environment ()
cook e = liftIO . threadDelay . round . (1000000 *) =<< expr e

eating :: Exp -> Statement -> Environment ()
eating cond task = do
  loopcond <- expr cond
  when (doubleBool loopcond) (eval task >> eating cond task)

expr :: Exp -> Environment Double
expr (Constant c) = return c
expr (Variable v) = environmentGet v
expr (Binary op x y) = binExpr op x y
expr (Boolean op x y) = boolExpr op x y;
expr (Unary op x) = unaryExpr op x
expr (Relational op x y) = relExpr op x y;
expr RobotLineSensor = robotLineSensor
expr RobotUltrason = robotUltrason

hungry :: Exp -> Statement -> Statement -> Environment ()
hungry cond i e = expr cond >>= \c -> if doubleBool c then eval i else eval e

order :: String -> Exp -> Environment ()
order var v = void . environmentSet var =<< expr v

puke :: Exp -> Environment ()
puke e = liftIO . void . print =<< expr e

relExpr :: RelationalOp -> Exp -> Exp -> Environment Double
relExpr Greater x y = fmap boolDouble ((>) <$> expr x <*> expr y)
relExpr GrEquals x y = fmap boolDouble ((>=) <$> expr x <*> expr y)
relExpr Equals x y = fmap boolDouble ((==) <$> expr x <*> expr y)
relExpr Less x y = fmap boolDouble ((<) <$> expr x <*> expr y)
relExpr LtEquals x y = fmap boolDouble ((<=) <$> expr x <*> expr y)

robotDrive :: Bot.Direction -> Environment ()
robotDrive dir = do {d <- liftIO Bot.connect; liftIO $ Bot.motorDirection dir d;
                 liftIO $ Bot.close d;}

robotLed :: Bot.Led -> Color -> Environment ()
robotLed l col = do {rv <- expr r; gv <- expr g; bv <- expr b;
               d <- liftIO Bot.connect; liftIO $ Bot.led d l rv gv bv;
               liftIO $ Bot.close d} where (r, g, b) = col

robotLineSensor :: Environment Double
robotLineSensor = do {d <- liftIO Bot.connect; v <- liftIO $ Bot.lineSensor d;
                liftIO $ Bot.close d; return v}

robotUltrason :: Environment Double
robotUltrason = do {d <- liftIO Bot.connect; v <- liftIO $ Bot.ultrason d;
                liftIO $ Bot.close d; return v}

sq :: [Statement] -> Environment ()
sq = foldr ((>>) . eval) (return ())

unaryExpr :: UnaryOp -> Exp -> Environment Double
unaryExpr Abs x = fmap abs (expr x)
unaryExpr Not x = expr x >>= \e -> return $ if e == 0 then 1 else 0
