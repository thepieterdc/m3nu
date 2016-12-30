module Main where

import Control.Monad.Trans.State.Lazy
import System.Environment

import Data.Map

import Evaluator
import Parser
import Types

main :: IO ((), EnvironmentVar)
main = do
  args <- getArgs
  if length args /= 1 then error "Usage: ./Main path_to_course.course: "
  else do
    ast <- parseFile $ head args
    runStateT (evaluate ast) (fromList [])
