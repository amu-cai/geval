module Main where

import GEval.Core
import GEval.OptionsParser

import System.Environment
import Options.Applicative

import Text.Printf

import System.IO
import System.Exit

import Data.Conduit.SmartSource

import System.FilePath

import Data.List (intercalate)

main :: IO ()
main = do
  args <- getArgs
  result <- runGEvalGetOptions args
  case result of
    Left parseResult -> handleParseResult parseResult >> return ()
    Right (opts, Just results) -> showTheResult opts results
    Right (_, Nothing) -> return ()

showTheResult :: GEvalOptions -> [(SourceSpec, [MetricValue])] -> IO ()
showTheResult opts [(_, vals)] = showTheResult' opts vals
showTheResult opts [] = error "no output given"
showTheResult opts multipleResults = showTable opts multipleResults

showTable :: GEvalOptions -> [(SourceSpec, [MetricValue])] -> IO ()
showTable opts multipleResults = do
  case metrics of
    [singleMetric] -> return ()
    [] -> error "no metric given"
    metrics -> putStrLn $ intercalate "\t" ("File name" : map show metrics)
  mapM_ (\entry -> putStrLn $ formatTableEntry opts entry)  multipleResults
  where metrics = gesMetrics $ geoSpec opts

formatTableEntry :: GEvalOptions -> (SourceSpec, [MetricValue]) -> String
formatTableEntry opts (sourceSpec, metrics) = intercalate "\t" (formatSourceSpec sourceSpec : vals)
   where vals = map (formatTheResult (gesPrecision $ geoSpec opts)) metrics

showTheResult' :: GEvalOptions -> [MetricValue] -> IO ()
-- do not show the metric if just one was given
showTheResult' opts [val] = putStrLn $ formatTheResult (gesPrecision $ geoSpec opts) val
showTheResult' opts [] = do
  hPutStrLn stderr "no metric given, use --metric option"
  exitFailure
showTheResult' opts vals =  mapM_ putStrLn $ map (formatTheMetricAndResult (gesPrecision $ geoSpec opts)) $ zip (gesMetrics $ geoSpec opts) vals

formatSourceSpec :: SourceSpec -> String
formatSourceSpec (FilePathSpec fp) = dropExtensions $ takeFileName fp
formatSourceSpec spec = show spec

formatTheMetricAndResult :: Maybe Int -> (Metric, MetricValue) -> String
formatTheMetricAndResult mPrecision (metric, val) = (show metric) ++ "\t" ++ (formatTheResult mPrecision val)


formatTheResult :: Maybe Int -> MetricValue -> String
formatTheResult Nothing  = show
formatTheResult (Just prec) = printf "%0.*f" prec
