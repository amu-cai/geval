{-# LANGUAGE QuasiQuotes #-}

module GEval.OptionsParser
       (fullOptionsParser,
        runGEval,
        runGEvalGetOptions,
        getOptions,
        metricReader,
        formatParser,
        readOptsFromConfigFile
        ) where

import Paths_geval (version)
import Data.Version (showVersion)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Options.Applicative
import qualified System.Directory as D
import System.FilePath
import System.Exit
import Data.Maybe
import System.IO
import Data.String.Here

import Data.Monoid ((<>))

import GEval.Core
import GEval.EvaluationScheme
import GEval.MetricsMeta (extraInfo, listOfAvailableEvaluationSchemes, allMetricsDescription, helpMetricParameterMetricsList)
import GEval.Common
import GEval.CreateChallenge
import GEval.LineByLine
import GEval.Submit (submit)
import GEval.BlackBoxDebugging
import GEval.Selector
import GEval.Validation
import GEval.Model
import GEval.ModelTraining

import Data.List (find, intercalate)
import Data.List.Utils (split)

import Data.Char (isSpace)

import Data.Conduit.SmartSource
import Data.CartesianStrings

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
                   <> help "Print a ranking of worst features, i.e. features that worsen the score significantly. Features are sorted using p-value for the Mann-Whitney U test comparing the items with a given feature and without it. For each feature the number of occurrences, average score and p-value is given." ))
                 <|>
                 (Diff <$> strOption
                    ( long "diff"
                      <> short 'd'
                      <> metavar "OTHER-OUT"
                      <> help "Compare results of evaluations (line by line) for two outputs."))
                <|>
                (MostWorseningFeatures <$> strOption
                    ( long "most-worsening-features"
                      <> short 'm'
                      <> help "Print a ranking of the \"most worsening\" features, i.e. features that worsen the score the most when comparing outputs from two systems."))
                <|>
                (flag' JustTokenize
                    ( long "just-tokenize"
                      <> short 'j'
                      <> help "Just tokenise standard input and print out the tokens (separated by spaces) on the standard output. rather than do any evaluation. The --tokenizer option must be given."))
                <|>
                (flag' Submit
                    ( long "submit"
                      <> short 'S'
                      <> help "Submit current solution for evaluation to an external Gonito instance specified with --gonito-host option. Optionally, specify --token."))
                <|>
                (flag' Validate
                    ( long "validate"
                      <> help "Validate challenge, it searches for potential errors in the given challenge path, like missing columns, files or format data."))
                <|>
                (flag' ListMetrics
                    ( long "list-metrics"
                      <> help "List all metrics with their descriptions"))
                <|>
                (flag' OracleItemBased
                    ( long "oracle-item-based"
                      <> help "Generate the best possible output considering outputs given by --out-file and --alt-out-file options (and peeking into the expected file)."))
                <|>
                (TrainModel <$> option auto
                    ( long "train"
                      <> metavar "MODEL-TYPE"
                      <> help "Train a model"))
                <|>
                (Infer <$> strOption
                    ( long "infer"
                      <> metavar "MODEL-PATH"
                      <> help "Infer from a model"))
                )

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
   <*> optional (strOption
                 ( long "filter"
                   <> metavar "FEATURE"
                   <> help "When in line-by-line or diff mode, show only items with a given feature"))
   <*> specParser
   <*> blackBoxDebuggingOptionsParser
   <*> optional (strOption
                 ( long "plot-graph"
                   <> metavar "FILE-PATH"
                   <> help "Plot an extra graph, applicable only for Probabilistic-MultiLabel/Soft-F-score (LOESS function for calibration)"))
   <*> switch
     ( long "mark-worst-features"
       <> help "Mark worst features when in the line-by-line mode")


formatParser :: Parser FormattingOptions
formatParser = FormattingOptions
    <$> (optional $ option auto ( long "precision"
      <> short 'p'
      <> metavar "NUMBER-OF-FRACTIONAL-DIGITS"
      <> help "Arithmetic precision, i.e. the number of fractional digits to be shown" ))
    <*> switch ( long "show-as-percentage"
      <> short '%'
      <> help "Returns the result as a percentage (i.e. maximum value of 100 instead of 1)" )

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
  <*> (optional $ selectorParser)
  <*> strOption
  ( long "out-file"
    <> short 'o'
    <> value defaultOutFile
    <> showDefault
    <> metavar "OUT"
    <> help "The name of the file to be evaluated" )
  <*> (optional $ some $ strOption
        ( long "alt-out-file"
          <> metavar "OUT"
          <> help "Alternative output file, makes sense only for some options, e.g. --oracle-item-based"))
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
  <*> (selectMetricsByName <$> selectMetricReader
                           <*> ((flip fromMaybe) <$> (singletonMaybe <$> altMetricReader) <*> metricReader))
  <*> formatParser
  <*> (optional $ option auto
       ( long "tokenizer"
         <> short 'T'
         <> metavar "TOKENIZER"
         <> help "Tokenizer on expected and actual output before running evaluation (makes sense mostly for metrics such BLEU), minimalistic, 13a, v14 and character-by-character tokenizers are implemented so far. Will be also used for tokenizing text into features when in --worst-features and --most-worsening-features modes." ))
  <*> ( optional . strOption $
        ( long "gonito-host"
          <> metavar "GONITO_HOST"
          <> help "Submit ONLY: Gonito instance location."
        )
      )
  <*> ( optional . strOption $
        ( long "token"
          <> metavar "TOKEN"
          <> help "Submit ONLY: Token for authorization with Gonito instance."
        )
      )
  <*> ( optional . strOption $
        ( long "gonito-git-annex-remote"
          <> metavar "GIT-ANNEX-REMOTE"
          <> help "Submit ONLY: Specification of a git-annex remote."
        )
      )
  <*> ( optional . strOption $
        ( long "references"
          <> metavar "FILE"
          <> help "External text file referenced"
        )
      )
  <*> ( optional $ option auto
        (long "bootstrap"
          <> short 'B'
          <> metavar "NUMBER-OF-SAMPLES"
          <> help "Tests on NUMBER-OF-SAMPLES bootstrap samples rather than just on the whole test set" ))
  <*> ( optional . strOption $
        ( long "in-header"
          <> metavar "FILE"
          <> help "One-line TSV file specifying a list of field names for input files"))
  <*> ( optional . strOption $
        ( long "out-header"
          <> metavar "FILE"
          <> help "One-line TSV file specifying a list of field names for output and expected files"))
   <*> switch
     ( long "show-preprocessed"
       <> help "When in --line-by-line or similar modes, not just work preprocessed data, but show them as such")

selectMetricsByName :: [String] -> [EvaluationScheme] -> [EvaluationScheme]
selectMetricsByName [] schemes = schemes
selectMetricsByName metricNames schemes = map (selectMetric schemes) metricNames
  where selectMetric schemes metricName = case find (\s -> evaluationSchemeName s == metricName) schemes of
          Just scheme -> scheme
          Nothing -> error $ "Cannot find metric named '" ++ metricName ++ "'"

defaultMinFrequency :: Integer
defaultMinFrequency = 1

selectorParser :: Parser Selector
selectorParser = parseSelector <$> (strOption $
                                    ( long "selector"
                                      <> metavar "JSON_PATH"
                                      <> help "Selector to an item to be considered"
                                    ))

blackBoxDebuggingOptionsParser :: Parser BlackBoxDebuggingOptions
blackBoxDebuggingOptionsParser = BlackBoxDebuggingOptions
  <$> option auto
    ( long "min-frequency"
      <> metavar "N"
      <> help "Minimum frequency for the worst features"
      <> value defaultMinFrequency
      <> showDefault)
  <*> switch
    ( long "word-shapes"
      <> help "Consider word shapes")
  <*> switch
    ( long "bigrams"
      <> help "Consider feature bigrams")
  <*> switch
    ( long "cartesian"
      <> help "Consider Cartesian combination of all features (computationally expensive!)")
  <*> optional (option auto
                ( long "min-cartesian-frequency"
                  <> metavar "N"
                  <> help "When combining features into Cartesian features, consider only features whose frequency exceeds the threshold given"))
  <*> switch
    ( long "numerical-features"
      <> help "Consider numerical features or field lengths")

singletonMaybe :: Maybe a -> Maybe [a]
singletonMaybe (Just x) = Just [x]
singletonMaybe Nothing = Nothing

sel :: Maybe Metric -> Metric -> Metric
sel Nothing m = m
sel (Just m) _ = m



metricReader :: Parser [EvaluationScheme]
metricReader = concatCartesianStrings <$>
  (many $ option auto         -- actually `some` should be used instead of `many`, the problem is that
    ( long "metric"           -- --metric might be in the config.txt file...
      <> short 'm'
      <> metavar "METRIC"
      <> help ("Metric to be used, e.g.:" ++ helpMetricParameterMetricsList)))

altMetricReader :: Parser (Maybe EvaluationScheme)
altMetricReader = optional $ option auto
               ( long "alt-metric"
                 <> short 'a'
                 <> metavar "METRIC"
                 <> help "Alternative metric (overrides --metric option)" )

selectMetricReader :: Parser [String]
selectMetricReader = many $ strOption
               ( long "select-metric"
                 <> metavar "METRIC-NAME"
                 <> help "Select metric(s) by name" )


runGEval :: [String] -> IO (Either (ParserResult GEvalOptions) (Maybe [(SourceSpec, [MetricResult])]))
runGEval args = do
  ret <- runGEvalGetOptions args
  case ret of
    Left e -> return $ Left e
    Right (_, mmv) -> return $ Right mmv

runGEvalGetOptions :: [String] -> IO (Either (ParserResult GEvalOptions) (GEvalOptions, Maybe [(SourceSpec, [MetricResult])]))
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

readOptsFromConfigFile :: [String] -> FilePath -> IO (Either (ParserResult GEvalOptions) GEvalOptions)
readOptsFromConfigFile args configFilePath = do
  configH <- openFile configFilePath ReadMode
  contents <- hGetContents configH
  getOptions' False ((parseConfigFileContents contents) ++ args)

data ConfigFileSymbol = Literal Char | Separator
  deriving (Eq, Show)

-- very simplistic handling of backslash escaping
-- even "\\" is treated as double backslashes...
parseConfigFileContents :: String -> [String]
parseConfigFileContents contents =
  filter (not . null)
  $ map (map (\(Literal c) -> c))
  $ split [Separator]
  $ parseSymbols contents
  where parseSymbols ('\\':c:t)
          | isSpace c = (Literal c) : parseSymbols t
          | otherwise = (Literal '\\'): (Literal c) :parseSymbols t
        parseSymbols (c:t)
          | isSpace c = Separator : parseSymbols t
          | otherwise = (Literal c) : parseSymbols t
        parseSymbols [] = []

attemptToReadOptsFromConfigFile :: [String] -> GEvalOptions -> IO (Either (ParserResult GEvalOptions) GEvalOptions)
attemptToReadOptsFromConfigFile args opts = do
  configExists <- D.doesFileExist configFilePath
  if configExists then do
      readOptsFromConfigFile args configFilePath
    else
      getOptions' False args
  where configFilePath = (getExpectedDirectory $ geoSpec opts) </> configFileName

runGEval'' :: GEvalOptions -> IO (Maybe [(SourceSpec, [MetricResult])])
runGEval'' opts = runGEval''' (geoSpecialCommand opts)
                              (geoResultOrdering opts)
                              (geoFilter opts)
                              (geoSpec opts)
                              (geoBlackBoxDebugginsOptions opts)
                              (geoGraphFile opts)
                              (geoMarkWorstFeatures opts)

runGEval''' :: Maybe GEvalSpecialCommand
              -> ResultOrdering
              -> Maybe String
              -> GEvalSpecification
              -> BlackBoxDebuggingOptions
              -> Maybe FilePath
              -> Bool
              -> IO (Maybe [(SourceSpec, [MetricResult])])
runGEval''' Nothing _ _ spec _ mGraphFile _ = do
  vals' <- geval spec
  let vals = map (\(s, val) -> (s, map getMetricValue val)) vals'
  case mGraphFile of
    Just graphFile -> do
      let graphsData = groupByMetric (gesMetrics spec) vals'
      mapM_ (\(ix, d) -> (plotGraph (getGraphFilename ix graphFile) d)) $ zip [0..] graphsData
    Nothing -> return ()
  return $ Just vals
runGEval''' (Just Init) _ _ spec _ _ _ = do
  initChallenge spec
  return Nothing
runGEval''' (Just PrintVersion) _ _ _ _ _ _ = do
  putStrLn ("geval " ++ showVersion version)
  return Nothing
runGEval''' (Just LineByLine) ordering featureFilter spec bbdo _ markWorstFeatures = do
  if markWorstFeatures
  then runLineByLineWithWorstFeatures ordering featureFilter spec bbdo
  else runLineByLine ordering featureFilter spec bbdo
  return Nothing
runGEval''' (Just WorstFeatures) ordering _ spec bbdo _ _ = do
  runWorstFeatures ordering spec bbdo
  return Nothing
runGEval''' (Just (Diff otherOut)) ordering featureFilter spec bbdo _ _ = do
  runDiff ordering featureFilter otherOut spec bbdo
  return Nothing
runGEval''' (Just (MostWorseningFeatures otherOut)) ordering _ spec bbdo _ _ = do
  runMostWorseningFeatures ordering otherOut spec bbdo
  return Nothing
runGEval''' (Just JustTokenize) _ _ spec _ _ _ = do
  justTokenize (gesTokenizer spec)
  return Nothing
runGEval''' (Just Submit) _ _ spec _ _ _ = do
  submit (gesGonitoHost spec) (gesToken spec) (gesGonitoGitAnnexRemote spec)
  return Nothing
runGEval''' (Just Validate) _ _ spec _ _ _ = do
  validateChallenge spec
  return Nothing
runGEval''' (Just ListMetrics) _ _ _ _ _ _ = do
  listMetrics
  return Nothing
runGEval''' (Just OracleItemBased) _ _ spec _ _ _ = do
  runOracleItemBased spec
  return Nothing
runGEval''' (Just (TrainModel modelType)) _ _ spec _ _ _ = do
  trainModel modelType spec
  return Nothing
runGEval''' (Just (Infer modelPath)) _ _ spec _ _ _ = do
  infer modelPath spec
  return Nothing

getGraphFilename :: Int -> FilePath -> FilePath
getGraphFilename 0 fp = fp
getGraphFilename ix fp = ((dropExtension fp) ++ "-" ++ (show ix)) ++ (takeExtension fp)

groupByMetric :: [EvaluationScheme]
                -> [(SourceSpec, [MetricOutput])]
                -> [(EvaluationScheme, [(SourceSpec, GraphSeries)])]
groupByMetric metrics results = filter (\(_, ss) -> not (null ss))
                                $ map extractMetric
                                $ zip [0..] metrics
  where extractMetric (ix, metric) =
            (metric, map (\(s, Just g) -> (s, g))
                     $ filter (\(s, mg) -> isJust mg)
                     $ map (\(s, out) -> (s, getGraphSeries out))
                     $ map (\(s, outs) -> (s, outs !! ix)) results)


plotGraph :: FilePath -> (EvaluationScheme, [(SourceSpec, GraphSeries)]) -> IO ()
plotGraph graphFile (scheme@(EvaluationScheme (ProbabilisticSoftFMeasure _) _), seriesSpecs) = do
  toFile def graphFile $ do
    layoutlr_title .= "GEval Graph / Calibration / Loess / " ++ (show scheme)
    let perfectSeries = (FilePathSpec "Perfect",
                         GraphSeries [(0.0, 0.0), (1.0, 1.0)])
    mapM_ plotOneSeries $ (perfectSeries : seriesSpecs)
  return ()
  where
    plotOneSeries :: (SourceSpec, GraphSeries) -> EC (LayoutLR Double Double Double) ()
    plotOneSeries (sspec, GraphSeries series) = plotLeft (line (recoverPath sspec) [series])
plotGraph _ _ = error "No graph for this metric!"


initChallenge :: GEvalSpecification -> IO ()
initChallenge spec = case gesExpectedDirectory spec of
  Nothing -> showInitInstructions
  Just expectedDirectory -> createChallenge True expectedDirectory spec

showInitInstructions = do
  putStrLn [here|
Run:
    geval --init --expected-directory CHALLENGE --metric METRIC-NAME --precision NUMBER-OF-DIGITS
to create a directory CHALLENGE representing a Gonito challenge.

(Note that `--out-directory` option is not taken into account with `--init` option.)
|]
  exitFailure


validateChallenge :: GEvalSpecification -> IO ()
validateChallenge spec = case gesExpectedDirectory spec of
  Nothing -> showValidateInstructions
  Just expectedDirectory -> validationChallenge expectedDirectory spec

showValidateInstructions = do
  putStrLn [here|
Run:
    geval --validate --expected-directory CHALLENGE
to validate a directory CHALLENGE representing a Gonito challenge.
|]
  exitFailure

listMetrics :: IO ()
listMetrics = putStrLn allMetricsDescription
