module Main where

import Utility (readInput, InputType (Real))
import Day19 (day19Part2)

main :: IO ()
main = do
  input <- readInput 19 Real
  print $ day19Part2 input
