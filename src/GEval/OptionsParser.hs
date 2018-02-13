{-# LANGUAGE QuasiQuotes #-}

module GEval.OptionsParser
       (fullOptionsParser,
        runGEval,
        runGEvalGetOptions,
        getOptions) where

import Options.Applicative
import qualified System.Directory as D
import System.FilePath
import System.Exit
import Data.Maybe
import System.IO
import Data.String.Here

import Data.Monoid ((<>))

import GEval.Core
import GEval.CreateChallenge
import GEval.LineByLine

fullOptionsParser = info (helper <*> optionsParser)
       (fullDesc
        <> progDesc "Run evaluation for tests in Gonito platform"
        <> header "geval - stand-alone evaluation tool for tests in Gonito platform")

optionsParser :: Parser GEvalOptions
optionsParser = GEvalOptions
   <$> optional ((flag' Init
                 ( long "init"
                   <> help "Init a sample Gonito challenge rather than run an evaluation" ))
                 <|>
                 (flag' LineByLine
                 ( long "line-by-line"
                   <> short 'l'
                   <> help "Give scores for each line rather than the whole test set" ))
                 <|>
                 (Diff <$> strOption
                    ( long "diff"
                      <> short 'd'
                      <> metavar "OTHER-OUT"
                      <> help "compare results")))
   <*> specParser

precisionArgParser :: Parser Int
precisionArgParser = option auto
    ( long "precision"
      <> short 'p'
      <> metavar "NUMBER-OF-FRACTIONAL-DIGITS"
      <> help "Arithmetic precision, i.e. the number of fractional digits to be shown" )

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
                  <> help "Directory with expected test results (the same as OUT-DIRECTORY, if not given)" ))
  <*> strOption
  ( long "test-name"
    <> short 't'
    <> value defaultTestName
    <> showDefault
    <> metavar "NAME"
    <> help "Test name (i.e. subdirectory with results or expected results)" )
  <*> strOption
  ( long "out-file"
    <> short 'o'
    <> value defaultOutFile
    <> showDefault
    <> metavar "OUT"
    <> help "The name of the file to be evaluated" )
  <*> strOption
  ( long "expected-file"
    <> short 'e'
    <> value defaultExpectedFile
    <> showDefault
    <> metavar "EXPECTED"
    <> help "The name of the file with expected results" )
  <*> strOption
  ( long "input-file"
    <> short 'i'
    <> value defaultInputFile
    <> showDefault
    <> metavar "INPUT"
    <> help "The name of the file with the input (applicable only for some metrics)" )
  <*> metricReader
  <*> optional precisionArgParser

metricReader :: Parser Metric
metricReader = option auto
               ( long "metric"
                 <> short 'm'
                 <> value defaultMetric
                 <> showDefault
                 <> metavar "METRIC"
                 <> help "Metric to be used - RMSE, MSE, Accuracy, F-measure (specify as F1, F2, F0.25, etc.), MAP, BLEU, NMI, ClippEU, LogLossHashed or CharMatch" )

runGEval :: [String] -> IO (Either (ParserResult GEvalOptions) (Maybe MetricValue))
runGEval args = do
  ret <- runGEvalGetOptions args
  case ret of
    Left e -> return $ Left e
    Right (_, mmv) -> return $ Right mmv

runGEvalGetOptions :: [String] -> IO (Either (ParserResult GEvalOptions) (GEvalOptions, Maybe MetricValue))
runGEvalGetOptions args = do
  optionExtractionResult <- getOptions args
  case optionExtractionResult of
    Left parserResult -> return $ Left parserResult
    Right opts -> do
      mmv <- runGEval'' opts
      return $ Right (opts, mmv)

getOptions :: [String] -> IO (Either (ParserResult GEvalOptions) GEvalOptions)
getOptions = getOptions' True

-- the first argument: whether to try to read from the config file
getOptions' :: Bool -> [String] -> IO (Either (ParserResult GEvalOptions) GEvalOptions)
getOptions' readOptsFromConfigFile args =
  case parserResult of
    Success opts -> if readOptsFromConfigFile then
                      attemptToReadOptsFromConfigFile args opts else
                        do
                          return $ Right opts
    otherwise -> return $ Left parserResult
  where parserResult = execParserPure (prefs idm) fullOptionsParser args

attemptToReadOptsFromConfigFile :: [String] -> GEvalOptions -> IO (Either (ParserResult GEvalOptions) GEvalOptions)
attemptToReadOptsFromConfigFile args opts = do
  configExists <- D.doesFileExist configFilePath
  if configExists then do
      configH <- openFile configFilePath ReadMode
      contents <- hGetContents configH
      getOptions' False ((words contents) ++ args)
    else
      getOptions' False args
  where configFilePath = (getExpectedDirectory $ geoSpec opts) </> configFileName


runGEval'' :: GEvalOptions -> IO (Maybe MetricValue)
runGEval'' opts = runGEval''' (geoSpecialCommand opts) (geoSpec opts)

runGEval''' :: Maybe GEvalSpecialCommand -> GEvalSpecification -> IO (Maybe MetricValue)
runGEval''' Nothing spec = do
  val <- geval spec
  return $ Just val
runGEval''' (Just Init) spec = do
  initChallenge spec
  return Nothing
runGEval''' (Just LineByLine) spec = do
  runLineByLine spec
  return Nothing
runGEval''' (Just (Diff otherOut)) spec = do
  runDiff otherOut spec
  return Nothing

initChallenge :: GEvalSpecification -> IO ()
initChallenge spec = case gesExpectedDirectory spec of
  Nothing -> showInitInstructions
  Just expectedDirectory -> createChallenge expectedDirectory spec

showInitInstructions = do
  putStrLn [here|
Run:
    geval --init --expected-directory CHALLENGE
to create a directory CHALLENGE representing a Gonito challenge.

You can specify a metric with `--metric METRIC-NAME` option.

Note that `--out-directory` option is not taken into account with `--init` option.
|]
  exitFailure
