module Main where

import Evaluator
import Parser

main :: IO ()
main = do
  ast <- parseFile "courses/test_order_and_puke.course"
  out <- evaluate ast newEnvironment
  print out
