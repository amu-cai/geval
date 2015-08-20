module Main where

import GEval
import System.Environment

main :: IO ()
main = do
  [expectedFilePath, outFilePath] <- getArgs
  result <- geval expectedFilePath outFilePath
  print $ result
