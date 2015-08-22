module OptionsParser
       (fullOptionsParser,
        runGEval) where

import Options.Applicative
import GEval

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
runGEval args =
  case parserResult of
    Success opts -> do
      val <- geval $ geoSpec opts
      return $ Right $ Just val
    otherwise -> return $ Left parserResult
  where parserResult = execParserPure (prefs idm) fullOptionsParser args
