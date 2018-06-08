module Main where

import GEval.Core
import GEval.OptionsParser

import System.Environment
import Options.Applicative

import Text.Printf

import System.IO
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  result <- runGEvalGetOptions args
  case result of
    Left parseResult -> handleParseResult parseResult >> return ()
    Right (opts, Just results) -> showTheResult opts results
    Right (_, Nothing) -> return ()

showTheResult :: GEvalOptions -> [MetricValue] -> IO ()
-- do not show the metric if just one was given
showTheResult opts [val] = putStrLn $ formatTheResult (gesPrecision $ geoSpec opts) val
showTheResult opts [] = do
  hPutStrLn stderr "no metric given, use --metric option"
  exitFailure

showTheResult opts vals =  mapM_ putStrLn $ map (formatTheMetricAndResult (gesPrecision $ geoSpec opts)) $ zip (gesMetrics $ geoSpec opts) vals

formatTheMetricAndResult :: Maybe Int -> (Metric, MetricValue) -> String
formatTheMetricAndResult mPrecision (metric, val) = (show metric) ++ "\t" ++ (formatTheResult mPrecision val)


formatTheResult :: Maybe Int -> MetricValue -> String
formatTheResult Nothing  = show
formatTheResult (Just prec) = printf "%0.*f" prec
