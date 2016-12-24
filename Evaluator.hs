module Evaluator where

import Environment
import Types

evaluateBoolExp :: BoolExp -> Environment -> IO Bool
evaluateBoolExp (BoolConst b) _ = return b
evaluateBoolExp (BoolNegate bExp) e = do { ex <- evaluateBoolExp bExp e; return $ not ex}

evaluatePrint :: String -> Environment -> IO ()
evaluatePrint txt _ = do { putStrLn txt; return ()}
