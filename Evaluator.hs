module Evaluator(module Evaluator, module Environment, module Types) where

import Environment
import Types

evaluate :: Statement -> Environment -> IO Environment
evaluate (Puke s) = evaluatePrint s

evaluateBoolExp :: BoolExp -> Environment -> IO Bool
evaluateBoolExp (BoolConst b) _ = return b
evaluateBoolExp (BoolNegate bExp) e = do { ex <- evaluateBoolExp bExp e; return $ not ex}

evaluatePrint :: String -> Environment -> IO Environment
evaluatePrint txt e = do { putStrLn txt; return e}
