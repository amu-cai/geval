module Main where

import GEval
import System.Environment

main :: IO ()
main = do
  [expectedFilePath, outFilePath] <- getArgs
  result <- gevalCore MSE expectedFilePath outFilePath
  print $ result
