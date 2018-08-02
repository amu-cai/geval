{-# LANGUAGE QuasiQuotes #-}

module GEval.OptionsParser
       (fullOptionsParser,
        runGEval,
        runGEvalGetOptions,
        getOptions) where

import Paths_geval (version)
import Data.Version (showVersion)

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

import Data.Conduit.SmartSource

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
                 (flag' PrintVersion
                 ( long "version"
                   <> short 'v'
                   <> help "Print GEval version" ))
                 <|>
                 (flag' LineByLine
                 ( long "line-by-line"
                   <> short 'l'
                   <> help "Give scores for each line rather than the whole test set" ))
                 <|>
                 (flag' WorstFeatures
                 ( long "worst-features"
                   <> short 'w'
                   <> help "Print a ranking of worst features, i.e. features that worsen the score significantly" ))
                 <|>
                 (Diff <$> strOption
                    ( long "diff"
                      <> short 'd'
                      <> metavar "OTHER-OUT"
                      <> help "compare results")))
   <*> ((flag' FirstTheWorst
         (long "sort"
          <> short 's'
          <> help "When in line-by-line or diff mode, sort the results from the worst to the best"))
        <|>
        (flag' FirstTheBest
         (long "reverse-sort"
          <> short 'r'
          <> help "When in line-by-line or diff mode, sort the results from the best to the worst"))
        <|> pure KeepTheOriginalOrder)
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
  <*> ((flip fromMaybe) <$> (singletonMaybe <$> altMetricReader) <*> metricReader)
  <*> optional precisionArgParser

singletonMaybe :: Maybe a -> Maybe [a]
singletonMaybe (Just x) = Just [x]
singletonMaybe Nothing = Nothing

sel :: Maybe Metric -> Metric -> Metric
sel Nothing m = m
sel (Just m) _ = m

metricReader :: Parser [Metric]
metricReader = many $ option auto         -- actually `some` should be used instead of `many`, the problem is that
               ( long "metric"            -- --metric might be in the config.txt file...
                 <> short 'm'
                 <> metavar "METRIC"
                 <> help "Metric to be used - RMSE, MSE, Accuracy, LogLoss, Likelihood, F-measure (specify as F1, F2, F0.25, etc.), MAP, BLEU, NMI, ClippEU, LogLossHashed, LikelihoodHashed, BIO-F1, BIO-F1-Labels or CharMatch" )

altMetricReader :: Parser (Maybe Metric)
altMetricReader = optional $ option auto
               ( long "alt-metric"
                 <> short 'a'
                 <> metavar "METRIC"
                 <> help "Alternative metric (overrides --metric option)" )

runGEval :: [String] -> IO (Either (ParserResult GEvalOptions) (Maybe [(SourceSpec, [MetricValue])]))
runGEval args = do
  ret <- runGEvalGetOptions args
  case ret of
    Left e -> return $ Left e
    Right (_, mmv) -> return $ Right mmv

runGEvalGetOptions :: [String] -> IO (Either (ParserResult GEvalOptions) (GEvalOptions, Maybe [(SourceSpec, [MetricValue])]))
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


runGEval'' :: GEvalOptions -> IO (Maybe [(SourceSpec, [MetricValue])])
runGEval'' opts = runGEval''' (geoSpecialCommand opts) (geoResultOrdering opts) (geoSpec opts)

runGEval''' :: Maybe GEvalSpecialCommand -> ResultOrdering -> GEvalSpecification -> IO (Maybe [(SourceSpec, [MetricValue])])
runGEval''' Nothing _ spec = do
  vals <- geval spec
  return $ Just vals
runGEval''' (Just Init) _ spec = do
  initChallenge spec
  return Nothing
runGEval''' (Just PrintVersion) _ _ = do
  putStrLn ("geval " ++ showVersion version)
  return Nothing
runGEval''' (Just LineByLine) ordering spec = do
  runLineByLine ordering spec
  return Nothing
runGEval''' (Just WorstFeatures) ordering spec = do
  runWorstFeatures ordering spec
  return Nothing
runGEval''' (Just (Diff otherOut)) ordering spec = do
  runDiff ordering otherOut spec
  return Nothing

initChallenge :: GEvalSpecification -> IO ()
initChallenge spec = case gesExpectedDirectory spec of
  Nothing -> showInitInstructions
  Just expectedDirectory -> createChallenge expectedDirectory spec

showInitInstructions = do
  putStrLn [here|
Run:
    geval --init --expected-directory CHALLENGE --metric METRIC-NAME --precision NUMBER-OF-DIGITS
to create a directory CHALLENGE representing a Gonito challenge.

(Note that `--out-directory` option is not taken into account with `--init` option.)
|]
  exitFailure
