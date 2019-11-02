module Main where

import GEval.Core
import GEval.EvaluationScheme
import GEval.Common
import GEval.OptionsParser
import GEval.ParseParams

import System.Environment
import Options.Applicative

import Text.Printf

import System.IO
import System.Exit

import Data.Conduit.SmartSource

import System.FilePath

import Data.List (intercalate, sort)

import qualified Data.Text as T

import Data.Map.Strict as M
import Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  result <- runGEvalGetOptions args
  case result of
    Left parseResult -> handleParseResult parseResult >> return ()
    Right (opts, Just results) -> showTheResult opts results
    Right (_, Nothing) -> return ()

showTheResult :: GEvalOptions -> [(SourceSpec, [MetricResult])] -> IO ()
showTheResult opts [(_, vals)] = showTheResult' opts vals
showTheResult opts [] = error "no output given"
showTheResult opts multipleResults = showTable opts multipleResults

showTable :: GEvalOptions -> [(SourceSpec, [MetricResult])] -> IO ()
showTable opts multipleResults = do
  let params = Prelude.map (\(ss, _) -> parseParamsFromSourceSpec ss) multipleResults

  let paramNames =
        sort
        $ S.toList
        $ S.unions
        $ Prelude.map (\(OutputFileParsed _ m) -> M.keysSet m)
        $ params

  case getHeader paramNames metrics of
    Just header -> putStrLn header
    Nothing -> return ()

  mapM_ (\entry -> putStrLn $ formatTableEntry opts paramNames entry) $ zip multipleResults params
  where metrics = gesMetrics $ geoSpec opts

getHeader :: [T.Text] -> [EvaluationScheme] -> Maybe String
getHeader [] [singleMetric] = Nothing
getHeader [] [] = error "no metric given"
getHeader [] schemes = Just $ intercalate "\t" ("File name" : Prelude.map evaluationSchemeName schemes)
getHeader params schemes = Just $ intercalate "\t" (Prelude.map T.unpack params
                                                    ++ Prelude.map evaluationSchemeName schemes)

formatTableEntry :: GEvalOptions -> [T.Text] -> ((SourceSpec, [MetricResult]), OutputFileParsed) -> String
formatTableEntry opts paramNames ((sourceSpec, metrics), ofParsed) = intercalate "\t" ((initialColumns paramNames sourceSpec ofParsed) ++ vals)
   where vals = Prelude.map (formatTheResult (gesPrecision $ geoSpec opts)) metrics

initialColumns :: [T.Text] -> SourceSpec -> OutputFileParsed -> [String]
initialColumns [] sourceSpec ofParsed = [formatSourceSpec sourceSpec]
initialColumns params sourceSpec (OutputFileParsed _ paramMap) =
  Prelude.map (\p -> T.unpack $ M.findWithDefault (T.pack "") p paramMap) params

showTheResult' :: GEvalOptions -> [MetricResult] -> IO ()
-- do not show the metric if just one was given
showTheResult' opts [val] = putStrLn $ formatTheResult (gesPrecision $ geoSpec opts) val
showTheResult' opts [] = do
  hPutStrLn stderr "no metric given, use --metric option"
  exitFailure
showTheResult' opts vals =  mapM_ putStrLn $ Prelude.map (formatTheMetricAndResult (gesPrecision $ geoSpec opts)) $ zip (gesMetrics $ geoSpec opts) vals

formatSourceSpec :: SourceSpec -> String
formatSourceSpec (FilePathSpec fp) = dropExtensions $ takeFileName fp
formatSourceSpec spec = show spec

formatTheMetricAndResult :: Maybe Int -> (EvaluationScheme, MetricResult) -> String
formatTheMetricAndResult mPrecision (scheme, val) = (evaluationSchemeName scheme) ++ "\t" ++ (formatTheResult mPrecision val)


formatTheResult :: Maybe Int -> MetricResult -> String
formatTheResult mPrecision (SimpleRun val) = formatSimpleResult mPrecision val

formatSimpleResult :: Maybe Int -> MetricValue -> String
formatSimpleResult Nothing = show
formatSimpleResult (Just prec) = printf "%0.*f" prec
