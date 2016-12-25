module Main where

import Evaluator
import Parser

main :: IO ()
main = do
  ast <- parseFile "courses/test_order.course"
  out <- evaluate ast newEnvironment
  print out
