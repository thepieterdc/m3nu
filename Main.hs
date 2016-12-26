module Main where

import Evaluator
import Parser

main :: IO ()
main = do
  --ast <- parseFile "courses/test_order_and_puke.course"
  ast <- parseString "order y 5;puke y;" -- WERKT DUS ZIJN WHITESPACE BUGS HOERA
  out <- evaluate ast newEnvironment
  print out
