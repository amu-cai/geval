{-# LANGUAGE QuasiQuotes #-}

module GEval.OptionsParser
       (fullOptionsParser,
        runGEval) where

import Options.Applicative
import qualified System.Directory as D
import System.FilePath
import System.Exit
import Data.Maybe
import System.IO
import Data.String.Here

import GEval.Core
import GEval.CreateChallenge

fullOptionsParser = info (helper <*> optionsParser)
       (fullDesc
        <> progDesc "Run evaluation for tests in Gonito platform"
        <> header "geval - stand-alone evaluation tool for tests in Gonito platform")

optionsParser :: Parser GEvalOptions
optionsParser = GEvalOptions
   <$> switch
      ( long "init"
         <> help "Init a sample Gonito challange rather than run an evaluation" )
   <*> specParser

specParser :: Parser GEvalSpecification
specParser = GEvalSpecification
  <$> strOption
  ( long "out-directory"
    <> value defaultOutDirectory
    <> showDefault
    <> metavar "OUT-DIRECTORY"
    <> help "Directory with test results to be evaluated" )
  <*> optional (strOption
                ( long "expected-directory"
                  <> metavar "EXPECTED-DIRECTORY"
                  <> help "Directory with expected test results (if not specified the same as OUT-DIRECTORY)" ))
  <*> strOption
  ( long "test-name"
    <> value defaultTestName
    <> showDefault
    <> metavar "NAME"
    <> help "Test name (i.e. subdirectory with results or expected results)" )
  <*> strOption
  ( long "out-file"
    <> value defaultOutFile
    <> showDefault
    <> metavar "OUT"
    <> help "The name of the file to be evaluated" )
  <*> strOption
  ( long "expected-file"
    <> value defaultExpectedFile
    <> showDefault
    <> metavar "EXPECTED"
    <> help "The name of the file with expected results" )
  <*> metricReader

metricReader :: Parser Metric
metricReader = option auto
               ( long "metric"
                 <> value defaultMetric
                 <> showDefault
                 <> metavar "METRIC"
                 <> help "Metric to be used" )

runGEval :: [String] -> IO (Either (ParserResult GEvalOptions) (Maybe MetricValue))
runGEval = runGEval' True

runGEval' :: Bool -> [String] -> IO (Either (ParserResult GEvalOptions) (Maybe MetricValue))
runGEval' readOptsFromConfigFile args =
  case parserResult of
    Success opts -> if readOptsFromConfigFile then
                      attemptToReadOptsFromConfigFile args opts else
                      Right <$> runGEval'' opts
    otherwise -> return $ Left parserResult
  where parserResult = execParserPure (prefs idm) fullOptionsParser args

attemptToReadOptsFromConfigFile :: [String] -> GEvalOptions -> IO (Either (ParserResult GEvalOptions) (Maybe MetricValue))
attemptToReadOptsFromConfigFile args opts = do
  configExists <- D.doesFileExist configFilePath
  if configExists then do
      configH <- openFile configFilePath ReadMode
      contents <- hGetContents configH
      runGEval' False ((words contents) ++ args)
    else
      runGEval' False args
  where configFilePath = (getExpectedDirectory $ geoSpec opts) </> configFileName


runGEval'' :: GEvalOptions -> IO (Maybe MetricValue)
runGEval'' opts = runGEval''' (geoInit opts) (geoSpec opts)

runGEval''' :: Bool -> GEvalSpecification -> IO (Maybe MetricValue)
runGEval''' True spec = do
  initChallange spec
  return Nothing
runGEval''' False spec = do
  val <- geval spec
  return $ Just val

initChallange :: GEvalSpecification -> IO ()
initChallange spec = case gesExpectedDirectory spec of
  Nothing -> showInitInstructions
  Just expectedDirectory -> createChallenge expectedDirectory spec

showInitInstructions = do
  putStrLn [here|
Run:
    geval --init --expected-directory CHALLANGE
to create a directory CHALLANGE representing a Gonito challange.

You can specify a metric with `--metric METRIC-NAME` option.

Note that `--out-directory` option is not taken into account with `--init` option.
|]
  exitFailure
