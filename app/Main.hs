module Main where

import GEval.Core
import GEval.OptionsParser

import System.Environment
import Options.Applicative

import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  result <- runGEvalGetOptions args
  case result of
    Left parseResult -> handleParseResult parseResult >> return ()
    Right (opts, Just result) -> showTheResult opts result
    Right (_, Nothing) -> return ()

showTheResult :: GEvalOptions -> MetricValue -> IO ()
showTheResult opts val = putStrLn $ formatTheResult (geoPrecision opts) val

formatTheResult :: Maybe Int -> MetricValue -> String
formatTheResult Nothing  = show
formatTheResult (Just prec) = printf "%0.*f" prec
