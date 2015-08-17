module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [expectedFilePath, outFilePath] <- getArgs
  result <- geval expectedFilePath outFilePath
  print $ result
