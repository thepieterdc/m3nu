module Evaluator(module Evaluator, module Environment, module Types) where

import Environment
import Types

evaluate :: Statement -> Environment -> IO Environment
evaluate (Puke s) = evaluatePuke s

evaluateBoolExp :: BoolExp -> Environment -> IO Bool
evaluateBoolExp (BoolConst b) _ = return b
evaluateBoolExp (BoolNegate bExp) e = do { ex <- evaluateBoolExp bExp e; return $ not ex}

evaluatePuke :: String -> Environment -> IO Environment
evaluatePuke txt e = do { putStrLn txt; return e}
