module Main where

import System.Environment

import Evaluator
import Parser

main :: IO Environment
main = do
  args <- getArgs
  if length args /= 1 then error "Usage: ./Main path_to_course.course"
  else do
    ast <- parseFile $ head args;
    print $ runState (evaluate ast) ()
