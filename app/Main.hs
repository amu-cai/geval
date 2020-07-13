module Main where

import GEval.Core
import GEval.EvaluationScheme
import GEval.Common
import GEval.OptionsParser
import GEval.ParseParams
import GEval.Formatting

import System.Environment
import Options.Applicative

import System.IO
import System.Exit

import Data.Conduit.SmartSource

import Data.SplitIntoCrossTabs

import System.FilePath

import Data.List (intercalate, sort)

import qualified Data.Text as T

import Data.Map.Strict as M
import qualified Data.Map.Lazy as LM
import Data.Set as S

import Data.Conduit.Bootstrap (defaultConfidenceLevel, getConfidenceBounds)

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
   where vals = Prelude.map (formatTheResult (gesFormatting $ geoSpec opts)) metrics

initialColumns :: [T.Text] -> SourceSpec -> OutputFileParsed -> [String]
initialColumns [] sourceSpec ofParsed = [formatSourceSpec sourceSpec]
initialColumns params sourceSpec (OutputFileParsed _ paramMap) =
  Prelude.map (\p -> T.unpack $ M.findWithDefault (T.pack "") p paramMap) params

showTheResult' :: GEvalOptions -> [MetricResult] -> IO ()
-- do not show the metric if just one was given
showTheResult' opts [val] = putStrLn $ formatTheResult (gesFormatting $ geoSpec opts) val
showTheResult' opts [] = do
  hPutStrLn stderr "no metric given, use --metric option"
  exitFailure
showTheResult' opts vals =  mapM_ putStrLn
                            $ intercalate [""]
                            $ Prelude.map (formatCrossTable (gesFormatting $ geoSpec opts))
                            $ splitIntoTablesWithValues (T.pack "metric") (T.pack "value") mapping metricLabels
  where mapping = LM.fromList $ zip metricLabels vals
        metricLabels = Prelude.map T.pack $ Prelude.map evaluationSchemeName $ gesMetrics $ geoSpec opts

formatCrossTable :: FormattingOptions -> TableWithValues MetricResult -> [String]
formatCrossTable format (TableWithValues [_, _] body) =
  -- actually we won't print metric/value header
  -- (1) to keep backward-compatible with the previous version
  -- (2) to be concise
  Prelude.map (formatCrossTableLine format) body
formatCrossTable format (TableWithValues header body) =
  (intercalate "\t" $ Prelude.map T.unpack header) : Prelude.map (formatCrossTableLine format) body



formatCrossTableLine :: FormattingOptions-> (T.Text, [MetricResult]) -> String
formatCrossTableLine format (rowName, values) =
  intercalate "\t" ((T.unpack rowName):Prelude.map (formatTheResult format) values)

formatSourceSpec :: SourceSpec -> String
formatSourceSpec (FilePathSpec fp) = dropExtensions $ takeFileName fp
formatSourceSpec spec = show spec

formatTheMetricAndResult :: FormattingOptions -> (EvaluationScheme, MetricResult) -> String
formatTheMetricAndResult format (scheme, val) = (evaluationSchemeName scheme) ++ "\t" ++ (formatTheResult format val)
