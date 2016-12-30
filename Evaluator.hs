module Evaluator(eval) where

import Control.Concurrent(threadDelay)
import Control.Monad
import Control.Monad.IO.Class
import qualified MBotPlus as Bot

import Types
import Utils

eval :: Statement -> Environment ()
eval (Cook a) = cook a
eval (Debug s) = debug s
eval (Eating cond s) = eating cond s
eval (Hungry cond ifc elsec) = hungry cond ifc elsec
eval (Order val var) = order val var
eval (Puke v) = puke v
eval Review = return ()
eval (RobotDrive d) = robotDrive d
eval (RobotLeds l c) = robotLed l c
eval (Seq s) = sq s

binaryExpr :: BinaryOp -> Exp -> Exp -> Environment Double
binaryExpr Add x y = do {xe <- expr x; ye <- expr y; return $ xe+ye}
binaryExpr Minus x y = do {xe <- expr x; ye <- expr y; return $ xe-ye}
binaryExpr Multiply x y = do{xe <- expr x; ye <- expr y; return $ xe*ye}
binaryExpr Divide x y = do {xe <- expr x; ye <- expr y; return $ xe/ye}

boolExpr :: BooleanOp -> Exp -> Exp -> Environment Double
boolExpr And x y = do {xe <- expr x; ye <- expr y;
                   return $ boolDouble $ xe /= 0 && ye /= 0}
boolExpr Or x y = do {xe <- expr x; ye <- expr y;
                  return $ boolDouble $ xe /= 0 || ye /= 0}

cook :: Exp -> Environment ()
cook amtexp = do {amt <- expr amtexp;
              liftIO $ threadDelay $ round $ amt*1000000; return ()}

debug :: String -> Environment ()
debug txt = do {liftIO $ print txt; return ()}

eating :: Exp -> Statement -> Environment ()
eating cond task = do
  loopcond <- expr cond
  when (doubleBool loopcond) $ do {_ <- eval task; eating cond task}

expr :: Exp -> Environment Double
expr (Constant c) = return c
expr (Variable v) = environmentGet v
expr (Binary op x y) = binaryExpr op x y
expr (Boolean op x y) = boolExpr op x y;
expr (Unary op x) = unaryExpr op x
expr (Relational op x y) = relationalExpr op x y;
expr RobotLineSensor = robotLineSensor
expr RobotUltrason = robotUltrason

hungry :: Exp -> Statement -> Statement -> Environment ()
hungry cond i e = do {c <- expr cond; if doubleBool c then eval i else eval e}

order :: String -> Exp -> Environment ()
order var v = do {x <- expr v; environmentSet var x; return ()}

puke :: Exp -> Environment ()
puke e = do {val <- expr e; liftIO $ print val; return ()}

relationalExpr :: RelationalOp -> Exp -> Exp -> Environment Double
relationalExpr Greater x y = do {xe <- expr x; ye <- expr y;
                             return $ boolDouble $ xe > ye}
relationalExpr GrEquals x y = do {xe <- expr x; ye <- expr y;
                              return $ boolDouble $ xe >= ye}
relationalExpr Equals x y = do {xe <- expr x; ye <- expr y;
                            return $ boolDouble $ xe == ye}
relationalExpr Less x y = do {xe <- expr x; ye <- expr y;
                          return $ boolDouble $ xe < ye}
relationalExpr LtEquals x y = do {xe <- expr x; ye <- expr y;
                              return $ boolDouble $ xe <= ye}

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
unaryExpr Abs x = do {e <- expr x; return $ if e < 0 then -e else e}
unaryExpr Not x = do {e <- expr x; return $ if e == 0 then 1 else 0}
