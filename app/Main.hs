module Main where

import GEval
import System.Environment

import Options.Applicative
import OptionsParser

main :: IO ()
main = do
  args <- getArgs
  result <- runGEval args
  case result of
    Left parseResult -> handleParseResult parseResult >> return ()
    Right (Just result) -> print $ result
    Right Nothing -> return ()
