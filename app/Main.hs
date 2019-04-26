module Main where

import System.Check
import System.Environment

main = do
  args <- getArgs
  runAll (args /= [])
