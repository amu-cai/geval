{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}


module GEval.Core
    ( geval,
      gevalCore,
      gevalCoreOnSingleLines,
      module GEval.Metric,
      LineInFile(..),
      isBetter,
      isBetterOrEqual,
      GEvalSpecialCommand(..),
      GEvalSpecification(..),
      ResultOrdering(..),
      GEvalOptions(..),
      defaultGEvalSpecification,
      defaultOutDirectory,
      defaultTestName,
      defaultOutFile,
      defaultExpectedFile,
      defaultInputFile,
      defaultMetric,
      getExpectedDirectory,
      configFileName,
      ParsedRecord(..),
      WithoutInput(..),
      WithInput(..),
      EvaluationContext(..),
      ParserSpec(..),
      fileAsLineSource,
      checkAndGetFiles,
      checkAndGetFilesSingleOut,
      checkMultipleOuts,
      checkMultipleOutsCore,
      gesMainMetric,
      gesMainScheme,
      gesPreprocess,
      getDataDecoder,
      threeLineSource,
      extensionsHandled,
      isEmptyFile,
      FileProcessingOptions(..),
      readHeaderFileWrapper,
      getInHeader,
      getOutHeader
    ) where

import Debug.Trace

import Data.Singletons.TH

import GEval.Metric
import GEval.MetricsMechanics
import GEval.EvaluationScheme

import Data.Conduit
import Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import Data.Text
import Data.Text.Read as TR
import Control.Applicative
import Control.Exception
import Control.Conditional (unlessM, whenM)
import qualified System.Directory as D
import System.Posix
import System.FilePath
import Data.Maybe
import Data.Either (rights)
import Data.Tuple
import qualified Data.List.Split as DLS
import Data.List (sortBy, isSuffixOf, minimum, maximum)
import Text.NaturalComp

import Control.Monad.IO.Class
import Control.Monad ((<=<), filterM)

import Data.Attoparsec.Text (parseOnly)

import Data.Conduit.SmartSource
import Data.Conduit.Header

import qualified Data.IntSet as IS

import GEval.BLEU
import GEval.Common
import GEval.Clippings
import GEval.PrecisionRecall
import GEval.ClusteringMetrics
import GEval.LogLossHashed
import GEval.CharMatch
import GEval.BIO
import GEval.ProbList
import GEval.WER
import Data.Conduit.AutoDecompress
import Text.Tokenizer
import GEval.Selector
import GEval.Annotation
import GEval.BlackBoxDebugging
import Data.Conduit.Bootstrap
import GEval.DataSource

import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

import qualified Data.Vector.Unboxed as DVU

import Statistics.Correlation

import Data.Statistics.Calibration (softCalibration)
import Data.Statistics.Loess (clippedLoess)

import Data.Proxy

import Data.Word

import "Glob" System.FilePath.Glob


isBetterOrEqual :: Metric -> MetricValue -> MetricValue -> Bool
isBetterOrEqual metric valA valB = not (isBetter metric valB valA)

isBetter :: Metric -> MetricValue -> MetricValue -> Bool
isBetter metric valA valB = isBetter' metricOrdering valA valB
  where isBetter' TheHigherTheBetter valA valB = valA > valB
        isBetter' TheLowerTheBetter valA valB = valA < valB
        metricOrdering = getMetricOrdering metric

isInputNeeded :: Metric -> Bool
isInputNeeded CharMatch = True
isInputNeeded _ = False

-- | Could output be preprocessable
isPreprocessable :: Metric -> Bool
isPreprocessable BLEU = True
isPreprocessable GLEU = True
isPreprocessable WER = True
isPreprocessable Accuracy = True
isPreprocessable CharMatch = True
isPreprocessable TokenAccuracy = True
isPreprocessable _ = False

defaultOutDirectory = "."
defaultTestName = "test-A"
defaultOutFile = "out.tsv"
defaultExpectedFile = "expected.tsv"
defaultInputFile = "in.tsv"

defaultMetric :: Metric
defaultMetric = RMSE

configFileName :: FilePath
configFileName = "config.txt"

-- | Specification of an evaluation task to be done.
data GEvalSpecification = GEvalSpecification
                          { gesOutDirectory :: FilePath,
                            gesExpectedDirectory :: Maybe FilePath,
                            gesTestName :: String,
                            gesSelector :: Maybe Selector,
                            gesOutFile :: String,
                            gesAltOutFiles :: Maybe [String],
                            gesExpectedFile :: String,
                            gesInputFile :: String,
                            gesMetrics :: [EvaluationScheme],
                            gesPrecision :: Maybe Int,
                            gesTokenizer :: Maybe Tokenizer,
                            gesGonitoHost :: Maybe String,
                            gesToken :: Maybe String,
                            gesGonitoGitAnnexRemote :: Maybe String,
                            gesReferences :: Maybe String,
                            gesBootstrapResampling :: Maybe Int,
                            gesInHeader :: Maybe String,
                            gesOutHeader :: Maybe String }


gesMainMetric :: GEvalSpecification -> Metric
gesMainMetric spec = case gesMetrics spec of
  (scheme:_) -> evaluationSchemeMetric scheme
  otherwise -> error "no metric given"

gesMainScheme :: GEvalSpecification -> EvaluationScheme
gesMainScheme spec = case gesMetrics spec of
  (scheme:_) -> scheme
  otherwise -> error "no metric given"

gesPreprocess :: GEvalSpecification -> (Text -> Text)
gesPreprocess spec = tokenizeTabSeparatedWithSpaces (gesTokenizer spec)

getExpectedDirectory :: GEvalSpecification -> FilePath
getExpectedDirectory spec = fromMaybe outDirectory $ gesExpectedDirectory spec
                            where outDirectory = gesOutDirectory spec

getInHeader :: GEvalSpecification -> Maybe FilePath
getInHeader spec = getHeader spec gesInHeader

getOutHeader :: GEvalSpecification -> Maybe FilePath
getOutHeader spec = getHeader spec gesOutHeader

getHeader spec selector = case selector spec of
  Just headerFile -> Just $ getExpectedDirectory spec </> headerFile
  Nothing -> Nothing

-- | Special command, not just running the regular evaluation.
-- See OptionsParser.hs for more information.
data GEvalSpecialCommand = Init
                           | LineByLine | LineByLineWithWorstFeatures | WorstFeatures
                           | Diff FilePath | MostWorseningFeatures FilePath
                           | PrintVersion | JustTokenize | Submit
                           | Validate | ListMetrics
                           | OracleItemBased

data ResultOrdering = KeepTheOriginalOrder | FirstTheWorst | FirstTheBest

data GEvalOptions = GEvalOptions
                    { geoSpecialCommand :: Maybe GEvalSpecialCommand,
                      geoResultOrdering :: ResultOrdering,
                      geoFilter :: Maybe String,
                      geoSpec :: GEvalSpecification,
                      geoBlackBoxDebugginsOptions :: BlackBoxDebuggingOptions,
                      geoGraphFile :: Maybe FilePath,
                      geoMarkWorstFeatures :: Bool }


defaultGEvalSpecification = GEvalSpecification {
  gesOutDirectory = defaultOutDirectory,
  gesExpectedDirectory = Nothing,
  gesTestName = defaultTestName,
  gesSelector = Nothing,
  gesOutFile = defaultOutFile,
  gesAltOutFiles = Nothing,
  gesExpectedFile = defaultExpectedFile,
  gesInputFile = defaultInputFile,
  gesMetrics = [EvaluationScheme defaultMetric []],
  gesPrecision = Nothing,
  gesTokenizer = Nothing,
  gesGonitoHost = Nothing,
  gesToken = Nothing,
  gesGonitoGitAnnexRemote = Nothing,
  gesReferences = Nothing,
  gesBootstrapResampling = Nothing,
  gesInHeader = Nothing,
  gesOutHeader = Nothing }

isEmptyFile :: FilePath -> IO (Bool)
isEmptyFile path = do
    stat <- getFileStatus path
    return ((fileSize stat) == 0)

-- | Extensions handled (tried) by default. Files with other
-- extensions are handled only when given explicitly.
-- Compressor extensions (e.g. "gz") should not be given here.
extensionsHandled :: [String]
extensionsHandled = ["tsv", "jsonl"]

data LineSource m = LineSource (ConduitT () Text m ()) (Text -> ItemTarget) (Text -> Text) SourceSpec Word32

geval :: GEvalSpecification -> IO [(SourceSpec, [MetricOutput])]
geval gevalSpec = do
  mInHeader <- readHeaderFileWrapper $ getInHeader gevalSpec
  mOutHeader <- readHeaderFileWrapper $ getOutHeader gevalSpec
  (inputSource, expectedSource, outSources) <- checkAndGetFiles False gevalSpec
  let chDataSource = ChallengeDataSource {
        challengeDataSourceInput = inputSource,
        challengeDataSourceExpected = expectedSource,
        challengeDataSourceSelector = gesSelector gevalSpec,
        challengeDataSourcePreprocess = gesPreprocess gevalSpec,
        challengeDataSourceFilter = Nothing,
        challengeDataSourceInHeader = mInHeader,
        challengeDataSourceOutHeader = mOutHeader }

  results <- Prelude.mapM (\outSource -> gevalOnSingleOut gevalSpec
                                                        DataSource {
                             dataSourceChallengeData = chDataSource,
                             dataSourceOut = outSource }) outSources
  return $ sortBy (\a b ->  (show $ fst a) `naturalComp` (show $ fst b)) results

noGraph :: d -> Maybe GraphSeries
noGraph = const Nothing

gevalOnSingleOut :: GEvalSpecification -> DataSource -> IO (SourceSpec, [MetricOutput])
gevalOnSingleOut gevalSpec dataSource = do
  vals <- Prelude.mapM (\scheme ->
                         gevalCore (evaluationSchemeMetric scheme)
                                   (gesBootstrapResampling gevalSpec)
                                   (addPreprocessing (applyPreprocessingOperations scheme) dataSource))
                                   schemes
  return (outSource, vals)
  where outSource = dataSourceOut dataSource
        schemes = gesMetrics gevalSpec

addPreprocessing :: (Text -> Text) -> DataSource -> DataSource
addPreprocessing prep dataSource =
  dataSource {
     dataSourceChallengeData = (dataSourceChallengeData dataSource) {
         challengeDataSourcePreprocess =
             (challengeDataSourcePreprocess $ dataSourceChallengeData dataSource) . prep }}

readHeaderFileWrapper :: Maybe FilePath -> IO (Maybe TabularHeader)
readHeaderFileWrapper Nothing = return Nothing
readHeaderFileWrapper (Just headerFilePath) = do
  mHeader <- readHeaderFile headerFilePath
  case mHeader of
    Just header -> return $ Just header
    Nothing -> throwM $ NoHeaderFile headerFilePath

checkAndGetFilesSingleOut :: Bool -> GEvalSpecification -> IO (SourceSpec, SourceSpec, SourceSpec)
checkAndGetFilesSingleOut forceInput gevalSpec = do
  res <- checkAndGetFiles forceInput gevalSpec
  case res of
    (inputSource, expectedSource, [outSource]) -> return (inputSource, expectedSource, outSource)
    _ -> throwM $ UnexpectedMultipleOutputs

checkAndGetFiles :: Bool -> GEvalSpecification -> IO (SourceSpec, SourceSpec, [SourceSpec])
checkAndGetFiles forceInput gevalSpec = do
  ess <- getSmartSourceSpec expectedTestDirectory defaultExpectedFile expectedFile
  case ess of
    Left NoSpecGiven -> throwM $ NoExpectedFile expectedFile
    Left (NoFile fp) -> throwM $ NoExpectedFile fp
    Left (NoDirectory d) -> do
       unlessM (D.doesDirectoryExist expectedDirectory) $ throwM $ NoExpectedDirectory expectedDirectory
       unlessM (D.doesDirectoryExist expectedTestDirectory) $ throwM $ NoExpectedTestDirectory expectedTestDirectory
       throwM $ NoExpectedDirectory d
    Right expectedSource -> do
       -- in most cases inputSource is NoSource (unless needed by a metric or in the line-by-line mode)
       inputSource <- getInputSourceIfNeeded forceInput (Prelude.map evaluationSchemeMetric schemes) expectedTestDirectory inputFile

       mMultipleOuts <- checkMultipleOuts gevalSpec
       osss <- case mMultipleOuts of
         Just filePaths -> return $ Prelude.map (\fp -> FilePathSpec fp) filePaths
         Nothing -> do
           oss <- checkSingleOut outTestDirectory outFile
           case oss of
             Left NoSpecGiven -> throwM $ NoOutFile outFile
             Left (NoFile fp) -> throwM $ NoOutFile fp
             Left (NoDirectory d) -> do
               unlessM (D.doesDirectoryExist outDirectory) $ throwM $ NoOutDirectory outDirectory
               unlessM (D.doesDirectoryExist outTestDirectory) $ throwM $ NoOutTestDirectory outTestDirectory
               throwM $ NoOutFile outFile
             Right outSource -> do
               return [outSource]
       return (inputSource, expectedSource, osss)
    where expectedTestDirectory = expectedDirectory </> testName
          outTestDirectory = outDirectory </> testName
          expectedDirectory = getExpectedDirectory gevalSpec
          outDirectory = gesOutDirectory gevalSpec
          testName = gesTestName gevalSpec
          outFile = gesOutFile gevalSpec
          expectedFile = gesExpectedFile gevalSpec
          inputFile = gesInputFile gevalSpec
          schemes = gesMetrics gevalSpec

checkSingleOut :: FilePath -> FilePath -> IO (Either SmartSourceError SourceSpec)
checkSingleOut outTestDirectory outFile
  | outFile == defaultOutFile = do
      -- if the default output file name is used try alternative formats (e.g. jsonl)
      specs <- Prelude.mapM (\ext -> getSmartSourceSpec outTestDirectory defaultOutFile (outFile -<.> ext)) extensionsHandled
      return $ case rights specs of
                  [] -> Prelude.head specs
                  rspecs@_ -> Right $ Prelude.head rspecs
  | otherwise = getSmartSourceSpec outTestDirectory defaultOutFile outFile

checkMultipleOuts :: GEvalSpecification -> IO (Maybe [FilePath])
checkMultipleOuts gevalSpec = checkMultipleOutsCore outDirectory testName outFile
  where outFile = gesOutFile gevalSpec
        outDirectory = gesOutDirectory gevalSpec
        testName = gesTestName gevalSpec

-- | Looks for multiple output files.
checkMultipleOutsCore :: FilePath -> FilePath -> FilePath -> IO (Maybe [FilePath])
checkMultipleOutsCore outDirectory testName outFile = do
  -- if the out.tsv is there (possibly with an alternative extension,
  -- e.g. jsonl and compressed), just use it - but here we just check
  -- this (`Nothing` will be returned in such a case, anyway)
  outFilePaths <- Prelude.mapM (\ext -> lookForCompressedFiles (outTestDirectory </> outFile -<.> ext))
                              extensionsHandled
  isSimpleOutTheres <- Prelude.mapM D.doesFileExist outFilePaths
  let isSimpleOutThere = Prelude.and isSimpleOutTheres

  let patterns = [compile ("out-*" <.> dataExt ++ compressorExt) |
                          dataExt <- extensionsHandled,
                          compressorExt <- ("":compressedFilesHandled)]
  multipleOuts <- Prelude.concat <$> globDir patterns outTestDirectory

  if outFile == defaultOutFile && not isSimpleOutThere && multipleOuts /= []
    then
      return $ Just multipleOuts
    else
      return Nothing

  where outTestDirectory = outDirectory </> testName

getOutFile :: GEvalSpecification -> FilePath -> FilePath
getOutFile gevalSpec out = outDirectory </> testName </> out
  where outDirectory = gesOutDirectory gevalSpec
        testName = gesTestName gevalSpec

getInputSourceIfNeeded :: Bool -> [Metric] -> FilePath -> FilePath -> IO SourceSpec
getInputSourceIfNeeded forced metrics directory inputFilePath
   | forced || (Prelude.any isInputNeeded metrics) = do
       iss <- getSmartSourceSpec directory defaultInputFile inputFilePath
       case iss of
         Left NoSpecGiven -> throwM $ NoInputFile inputFilePath
         Left (NoFile fp) -> throwM $ NoInputFile fp
         Left (NoDirectory _) -> throwM $ NoInputFile inputFilePath
         Right sourceSpec -> return sourceSpec
   | otherwise = return NoSource

data FileProcessingOptions = FileProcessingOptions {
  fileProcessingOptionsSelector :: Maybe Selector,
  fileProcessingOptionsPreprocess :: (Text -> Text),
  fileProcessingOptionsHeader :: Maybe TabularHeader }


fileAsLineSource :: SourceSpec -> FileProcessingOptions -> LineSource (ResourceT IO)
fileAsLineSource spec options =
  LineSource ((smartSource spec) .| autoDecompress .| CT.decodeUtf8Lenient .| CT.lines .| processHeader mHeader) (select (getDataFormat spec) mSelector) preprocess spec 1
  where mSelector = fileProcessingOptionsSelector options
        preprocess = fileProcessingOptionsPreprocess options
        mHeader = fileProcessingOptionsHeader options

getDataDecoder :: LineSource (ResourceT IO) -> (Text -> ItemTarget)
getDataDecoder (LineSource _ dd _ _ _) = dd

getDataFormat :: SourceSpec -> DataFormat
getDataFormat (FilePathSpec filePath) = getDataFormatFromFilePath filePath
getDataFormat Stdin = Tsv
getDataFormat NoSource = Tsv
getDataFormat (Http url) = getDataFormatFromFilePath url
getDataFormat (Https url) = getDataFormatFromFilePath url

getDataFormatFromFilePath :: FilePath -> DataFormat
getDataFormatFromFilePath path =
   case takeExtensions path' of
        ".jsonl" -> Jsonl
        _ -> Tsv
   where  path' = if Prelude.or $ Prelude.map (\ext -> ext `Data.List.isSuffixOf` path)
                                              compressedFilesHandled
                  then dropExtension path
                  else path

dataDecoder fmt mSelector = CC.map (select fmt mSelector)

gevalCoreOnSingleLines :: Metric -> (Text -> Text) -> (Text -> ItemTarget) -> LineInFile -> (Text -> ItemTarget) -> LineInFile -> (Text -> ItemTarget) -> LineInFile -> IO (MetricOutput)
gevalCoreOnSingleLines metric preprocess inpDecoder inpLine expDecoder expLine outDecoder outLine =
  gevalCoreOnSources metric (singleLineAsLineSource inpLine inpDecoder preprocess)
                            (singleLineAsLineSource expLine expDecoder outputPreprocess)
                            (singleLineAsLineSource outLine outDecoder outputPreprocess)
  where outputPreprocess = if isPreprocessable metric
                           then preprocess
                           else id

singleLineAsLineSource :: LineInFile -> (Text -> ItemTarget) -> (Text -> Text) -> LineSource (ResourceT IO)
singleLineAsLineSource (LineInFile sourceSpec lineNo line) itemDecoder preprocess =
  LineSource (CL.sourceList [line]) itemDecoder preprocess sourceSpec lineNo

-- some metrics are handled by Bootstrap due to legacy issues,
-- fix on the way
handleBootstrap :: Metric -> Bool
handleBootstrap (Mean (MultiLabelFMeasure _)) = True
handleBootstrap (Mean _) = False
handleBootstrap CharMatch = False
handleBootstrap (LogLossHashed _) = False
handleBootstrap (LikelihoodHashed _ ) = False
handleBootstrap Pearson = False
handleBootstrap Spearman = False
handleBootstrap (ProbabilisticMultiLabelFMeasure beta) = False
handleBootstrap (ProbabilisticSoftFMeasure beta) = False
handleBootstrap _ = True

-- | Runs evaluation for a given metric using the sources specified
-- for input, expected output and output. Returns the metric value.
-- Throws @GEvalException@ if something was wrong in the data (e.g.
-- inconsistent number of lines in the sources).
gevalCore :: Metric           -- ^ evaluation metric
          -> (Maybe Int)      -- ^ number of bootstrap samples
          -> DataSource
          -> IO (MetricOutput) -- ^ metric value for the output against the expected output
gevalCore metric mBootstrapResampling dataSource = do
  whenM (isEmptyFileSource outSource) $ throwM $ EmptyOutput
  go metric
     (fileAsLineSource inputSource inOptions)
     (fileAsLineSource expectedSource outOptions)
     (fileAsLineSource outSource outOptions)
  where go = case mBootstrapResampling of
               Nothing -> gevalCoreOnSources
               Just bootstrapResampling -> if handleBootstrap metric
                                          then gevalBootstrapOnSources bootstrapResampling
                                          else gevalCoreOnSources
        outOptions = FileProcessingOptions {
          fileProcessingOptionsSelector = mSelector,
          fileProcessingOptionsPreprocess = preprocess,
          fileProcessingOptionsHeader = mOutHeader }
        inOptions = FileProcessingOptions {
          fileProcessingOptionsSelector = mSelector,
          fileProcessingOptionsPreprocess = preprocess,
          fileProcessingOptionsHeader = mInHeader }
        challengeDataSource = dataSourceChallengeData dataSource
        mSelector = challengeDataSourceSelector challengeDataSource
        preprocess = challengeDataSourcePreprocess challengeDataSource
        mInHeader = challengeDataSourceInHeader challengeDataSource
        mOutHeader = challengeDataSourceOutHeader challengeDataSource
        inputSource = challengeDataSourceInput challengeDataSource
        expectedSource = challengeDataSourceExpected challengeDataSource
        outSource = dataSourceOut dataSource



isEmptyFileSource :: SourceSpec -> IO Bool
isEmptyFileSource (FilePathSpec filePath) = isEmptyFile filePath
isEmptyFileSource _ = return False

logLossToLikehood logLoss = exp (-logLoss)

data LineInFile = LineInFile SourceSpec Word32 Text

gevalBootstrapOnSources :: (MonadIO m, MonadThrow m, MonadUnliftIO m) =>
                          Int                         -- ^ number of samples
                          -> Metric                    -- ^ evaluation metric
                          -> LineSource (ResourceT m)  -- ^ source of the input values
                          -> LineSource (ResourceT m)  -- ^ source to read the expected output
                          -> LineSource (ResourceT m)  -- ^ source to read the output
                          -> m (MetricOutput)           -- ^ metric values for the output against the expected output

-- for the time being hardcoded
gevalBootstrapOnSources numberOfSamples (Mean (MultiLabelFMeasure beta)) inputLineStream expectedLineStream outLineStream = do
  gevalRunPipeline parserSpec (trans step) finalPipeline context
  where parserSpec = (ParserSpecWithoutInput (liftOp expParser) (liftOp outParser))
        context = (WithoutInput expectedLineStream outLineStream)
        step = itemStep SAMultiLabelFMeasure
        expParser = expectedParser SAMultiLabelFMeasure
        outParser = outputParser SAMultiLabelFMeasure
        finalPipeline = fixer (
          CL.map (fMeasureOnCounts beta)
          .| (bootstrapC numberOfSamples
              $ continueGEvalCalculations SAMSE MSE))
        trans :: ((a, b) -> c) -> ParsedRecord (WithoutInput m a b) -> c
        trans step (ParsedRecordWithoutInput x y) = step (x, y)

gevalBootstrapOnSources numberOfSamples metric inputLineStream expectedLineStream outLineStream = do
    case toSing $ toHelper metric of
                SomeSing smetric -> gevalRunPipeline parserSpec (trans step) finalPipeline context
                                     where parserSpec = (ParserSpecWithoutInput (liftOp expParser) (liftOp outParser))
                                           context = (WithoutInput expectedLineStream outLineStream)
                                           step = itemStep smetric
                                           expParser = expectedParser smetric
                                           outParser = outputParser smetric
                                           finalPipeline = fixer (bootstrapC numberOfSamples $ continueGEvalCalculations smetric metric)
                                           trans :: ((a, b) -> c) -> ParsedRecord (WithoutInput m a b) -> c
                                           trans step (ParsedRecordWithoutInput x y) = step (x, y)

fixer :: ConduitT c Void (ResourceT m) [MetricOutput] -> ConduitT c Void (ResourceT m) MetricOutput
fixer c = do
  outputs <- c
  let values = Prelude.map (\(MetricOutput (SimpleRun v) _) -> v) outputs
  return $ MetricOutput (BootstrapResampling values) Nothing

-- | Runs evaluation for a given metric using the sources given
-- for input, expected output and output. Returns the metric value.
-- Throws @GEvalException@ if something was wrong in the data (e.g.
-- inconsistent number of lines in the sources).
--
-- The difference between this and @gevalCore@ is that it operates on Conduit
-- sources (rather than source specification).
--
-- Metrics are starting to be really defined here, though when the
-- input is not needed for doing the evaluation (which is not in most
-- cases), the work is delegated to @gevalCoreWithoutInput@ function.
gevalCoreOnSources :: (MonadIO m, MonadThrow m, MonadUnliftIO m) =>
                     Metric                      -- ^ evaluation metric
                     -> LineSource (ResourceT m)  -- ^ source of the input values
                     -> LineSource (ResourceT m)  -- ^ source to read the expected output
                     -> LineSource (ResourceT m)  -- ^ source to read the output
                     -> m (MetricOutput)           -- ^ metric values for the output against the expected output

-- first not-standard metrics

-- CharMatch is the only metric needing input lines (so far)
gevalCoreOnSources CharMatch inputLineSource = helper inputLineSource
 where
   helper inputLineSource expectedLineSource outputLineSource = do
     gevalCoreGeneralized (ParserSpecWithInput justUnpack justUnpack justUnpack) step countAgg (fMeasureOnCounts charMatchBeta) noGraph (WithInput inputLineSource expectedLineSource outputLineSource)
   step (ParsedRecordWithInput inp exp out) = getCharMatchCount inp exp out
   justUnpack = liftOp (Right . unpack)

gevalCoreOnSources (LogLossHashed nbOfBits) _ = helperLogLossHashed nbOfBits id
gevalCoreOnSources (LikelihoodHashed nbOfBits) _ = helperLogLossHashed nbOfBits logLossToLikehood


gevalCoreOnSources (Mean (MultiLabelFMeasure beta)) _
  = gevalCoreWithoutInputOnItemTargets (Right . intoWords)
                                       (Right . getWords)
                                       ((fMeasureOnCounts beta) . (getCounts (==)))
                                       averageC
                                       id
                                       noGraph
    where
      -- repeated as below, as it will be refactored into dependent types soon anyway
      getWords (RawItemTarget t) = Prelude.map unpack $ selectByStandardThreshold $ parseIntoProbList t
      getWords (PartiallyParsedItemTarget ts) = Prelude.map unpack ts
      intoWords (RawItemTarget t) = Prelude.map unpack $ Data.Text.words t
      intoWords (PartiallyParsedItemTarget ts) = Prelude.map unpack ts

gevalCoreOnSources (Mean WER) _
  = gevalCoreWithoutInputOnItemTargets (Right . intoWords)
                                       (Right . getWords)
                                       ((uncurry (/.)) . (uncurry werStep))
                                       averageC
                                       id
                                       noGraph
    where
      -- repeated as below, as it will be refactored into dependent types soon anyway
      getWords (RawItemTarget t) = Prelude.map unpack $ selectByStandardThreshold $ parseIntoProbList t
      getWords (PartiallyParsedItemTarget ts) = Prelude.map unpack ts
      intoWords (RawItemTarget t) = Prelude.map unpack $ Data.Text.words t
      intoWords (PartiallyParsedItemTarget ts) = Prelude.map unpack ts

gevalCoreOnSources (Mean _) _ = error $ "Mean/ meta-metric defined only for MultiLabel-F1 and WER for the time being"

-- only MultiLabel-F1 handled for JSONs for the time being...
gevalCoreOnSources (MultiLabelFMeasure beta) _ = gevalCoreWithoutInputOnItemTargets (Right . intoWords)
                                                                            (Right . getWords)
                                                                            (getCounts (==))
                                                                            countAgg
                                                                            (fMeasureOnCounts beta)
                                                                            noGraph
    where
      getWords (RawItemTarget t) = Prelude.map unpack $ selectByStandardThreshold $ parseIntoProbList t
      getWords (PartiallyParsedItemTarget ts) = Prelude.map unpack ts
      intoWords (RawItemTarget t) = Prelude.map unpack $ Data.Text.words t
      intoWords (PartiallyParsedItemTarget ts) = Prelude.map unpack ts

gevalCoreOnSources Pearson _ = gevalCoreByCorrelationMeasure pearson
gevalCoreOnSources Spearman _ = gevalCoreByCorrelationMeasure spearman

gevalCoreOnSources (ProbabilisticMultiLabelFMeasure beta) _ = generalizedProbabilisticFMeasure beta
                                                                                               SAProbabilisticMultiLabelFMeasure

gevalCoreOnSources (ProbabilisticSoftFMeasure beta) _ = generalizedProbabilisticFMeasure beta
                                                                                         SAProbabilisticSoftFMeasure

-- and now more typical metrics, which:
-- 1) parse the expected output
-- 2) parse the actual output
-- 3) compare the actual output and the expected output (for each record/line separately)
-- 4) aggregate the results
-- 5) apply some final funtion on the aggregate
-- 6) create a graph using the aggregate (applicable only to some metrics)

gevalCoreOnSources metric _ = gevalCoreOnSourcesStandardWay metric

gevalCoreOnSourcesStandardWay :: (MonadIO m, MonadThrow m, MonadUnliftIO m) =>
                     Metric                      -- ^ evaluation metric
                     -> LineSource (ResourceT m)  -- ^ source to read the expected output
                     -> LineSource (ResourceT m)  -- ^ source to read the output
                     -> m (MetricOutput)           -- ^ metric values for the output against the expected output
gevalCoreOnSourcesStandardWay metric expectedLineStream outLineStream =
  case toSing $ toHelper metric of
    SomeSing smetric -> gevalRunPipeline parserSpec (trans step) finalPipeline context
                       where parserSpec = (ParserSpecWithoutInput (liftOp expParser) (liftOp outParser))
                             context = (WithoutInput expectedLineStream outLineStream)
                             step = itemStep smetric
                             expParser = expectedParser smetric
                             outParser = outputParser smetric
                             finalPipeline = continueGEvalCalculations smetric metric
                             trans :: ((a, b) -> c) -> ParsedRecord (WithoutInput m a b) -> c
                             trans step (ParsedRecordWithoutInput x y) = step (x, y)

helperLogLossHashed nbOfBits finalStep expectedLineSource outLineSource =
          gevalCore''' (ParserSpecWithoutInput (liftOp (Right . id)) (liftOp tentativeParser)) (\(lineNo, (t,d)) -> calculateLogLoss nbOfBits lineNo t (parseDistributionWrapper nbOfBits lineNo d)) averageC (finalStep . negate) noGraph (WithoutInput expectedLineSource outLineSource)
  where -- Unfortunately, we're parsing the distribution twice. We need to
        -- tentatively parse the distribution when the line number is unknown
        -- (so we just set it to 1)
        -- @TODO Fix this.
        tentativeParser t = case parseDistribution nbOfBits 1 t of
          Right _ -> Right t
          Left m -> Left m

generalizedProbabilisticFMeasure beta metric = gevalCoreWithoutInput metric
                                               probabilisticSoftAgg
                                               (fMeasureOnProbabilisticCounts beta)
                                               loessGraph
  where probabilisticSoftAgg :: Monad m => ConduitM ([Double], [Double], Double, Int) o m ([Double], [Double], Double, Int)
        probabilisticSoftAgg = CC.foldl probabilisticSoftFolder ([], [], fromInteger 0, 0)
        probabilisticSoftFolder (r1, p1, g1, e1) (r2, p2, g2, e2) = (r1 ++ r2, p1 ++ p2, g1 + g2, e1 + e2)
        loessGraph :: ([Double], [Double], Double, Int) -> Maybe GraphSeries
        loessGraph (results, probs, _, _) = Just $ GraphSeries $ Prelude.map (\x -> (x, clippedLoess probs' results' x)) $ Prelude.filter (\p -> p > lowest && p < highest) $ Prelude.map (\d -> 0.01 * (fromIntegral d)) [1..99]
           where results' = DVU.fromList results
                 probs' = DVU.fromList probs
                 lowest = Data.List.minimum probs
                 highest = Data.List.maximum probs
        fMeasureOnProbabilisticCounts :: Double -> ([Double], [Double], Double, Int) -> Double
        fMeasureOnProbabilisticCounts beta (results, probs, got, nbExpected) = weightedHarmonicMean beta calibrationMeasure recall
           where calibrationMeasure = softCalibration results probs
                 recall = got /. nbExpected


countAgg :: (Num n, Num v, Monad m) => ConduitM (n, v, v) o m (n, v, v)
countAgg = CC.foldl countFolder (fromInteger 0, fromInteger 0, fromInteger 0)

countFragAgg :: (Num n, Num v, Monad m) => ConduitM (n, n, v, v) o m (n, n, v, v)
countFragAgg = CC.foldl countFragFolder (fromInteger 0, fromInteger 0, fromInteger 0, fromInteger 0)

gevalCoreByCorrelationMeasure :: (MonadUnliftIO m, MonadThrow m, MonadIO m) =>
                                (V.Vector (Double, Double) -> Double) -> -- ^ correlation function
                                LineSource (ResourceT m) ->  -- ^ source to read the expected output
                                LineSource (ResourceT m) ->  -- ^ source to read the output
                                m (MetricOutput)             -- ^ metric values for the output against the expected output
gevalCoreByCorrelationMeasure correlationFunction =
  gevalCoreWithoutInput SAPearson correlationC finalStep noGraph
  where correlationC = CC.foldl (flip (:)) []
        finalStep pairs = correlationFunction $ V.fromList pairs

parseDistributionWrapper :: Word32 -> Word32 -> Text -> HashedDistribution
parseDistributionWrapper nbOfBits seed distroSpec = case parseDistribution nbOfBits seed distroSpec of
  Right distro -> distro
  Left s -> throw $ UnexpectedData 0 s -- shouldn't be here anyway

data SourceItem a = Got a | Wrong String | Done

skipLineNumber :: (x -> c) -> ((Word32, x) -> c)
skipLineNumber fun = fun . snd

-- | A helper function to run evaluation when the input is not needed to calculate the metric value.
gevalCoreWithoutInput :: (MonadUnliftIO m, MonadThrow m, MonadIO m)
                      => SAMetric t
                      -> (ConduitT (ItemIntermediateRepresentationType t) Void (ResourceT m) d)  -- ^ a Conduit which aggregates all the combined values into
                                                   -- a "total" value
                      -> (d -> Double)             -- ^ function to transform the "total" value into the final score
                      -> (d -> Maybe GraphSeries)
                      -> LineSource (ResourceT m)  -- ^ source to read the expected output
                      -> LineSource (ResourceT m)  -- ^ source to read the output
                      -> m (MetricOutput)           -- ^ metric values for the output against the expected output
gevalCoreWithoutInput smetric aggregator finalStep generateGraph expectedLineStream outLineStream =
  gevalCoreWithoutInputOnItemTargets (liftOp expParser) (liftOp outParser) iStep aggregator finalStep generateGraph expectedLineStream outLineStream
  where expParser = expectedParser smetric
        outParser = outputParser smetric
        iStep = itemStep smetric

gevalCoreWithoutInputOnItemTargets :: (MonadUnliftIO m, MonadThrow m, MonadIO m)
                      => (ItemTarget -> Either String a) -- ^ parser for values in the expected output
                      -> (ItemTarget -> Either String b) -- ^ parser for values in the actual output
                      -> ((a, b) -> c)             -- ^ function which combines parsed values into a single value
                                                   -- (will be launched for each item, e.g. an error/cost function
                                                   -- could be calculated here)
                      -> (ConduitT c Void (ResourceT m) d)  -- ^ a Conduit which aggregates all the combined values into
                                                   -- a "total" value
                      -> (d -> Double)             -- ^ function to transform the "total" value into the final score
                      -> (d -> Maybe GraphSeries)
                      -> LineSource (ResourceT m)  -- ^ source to read the expected output
                      -> LineSource (ResourceT m)  -- ^ source to read the output
                      -> m (MetricOutput)           -- ^ metric values for the output against the expected output
gevalCoreWithoutInputOnItemTargets expParser outParser itemStep aggregator finalStep generateGraph expectedLineStream outLineStream =
  gevalCoreGeneralized (ParserSpecWithoutInput expParser outParser) (trans itemStep) aggregator finalStep generateGraph (WithoutInput expectedLineStream outLineStream)
 where
   trans :: ((a, b) -> c) -> ParsedRecord (WithoutInput m a b) -> c
   trans step (ParsedRecordWithoutInput x y) = step (x, y)



gevalCore''' :: (MonadUnliftIO m, MonadThrow m, MonadIO m) => ParserSpec (WithoutInput m a b) -> ((Word32, (a, b)) -> c) -> (ConduitT c Void (ResourceT m) d) -> (d -> Double) -> (d -> Maybe GraphSeries) -> WithoutInput m a b -> m (MetricOutput)
gevalCore''' parserSpec itemStep aggregator finalStep generateGraph context =
  gevalCoreGeneralized' parserSpec (trans itemStep) aggregator finalStep generateGraph context
 where
   trans :: ((Word32, (a, b)) -> c) -> (Word32, ParsedRecord (WithoutInput m a b)) -> c
   trans step (n, ParsedRecordWithoutInput x y) = step (n, (x, y))

-- | General function to run the evaluation, no matter which particular metric
-- was used. It could be seen as the "engine" to run the evaluation.
-- If you are defining a new metric, you usually don't have to change anything
-- here.
gevalCoreGeneralized :: (EvaluationContext ctxt m, MonadUnliftIO m, MonadThrow m, MonadIO m)
                        => ParserSpec ctxt                   -- ^ parsers to parse data
                        -> (ParsedRecord ctxt -> c)          -- ^ function to go from the parsed value into
                                                             -- some "local" score calculated for each line (item)
                        -> (ConduitT c Void (ResourceT m) d) -- ^ a Conduit to aggregate score into a "total"
                        -> (d -> Double)                     -- ^ function to transform the "total" value into the final score
                        -> (d -> Maybe GraphSeries)
                        -> ctxt                              -- ^ "context", i.e. 2 or 3 sources needed to operate
                        -> m MetricOutput
gevalCoreGeneralized parserSpec itemStep aggregator finalStep generateGraph context =
  gevalCoreGeneralized' parserSpec (skipLineNumber itemStep) aggregator finalStep generateGraph context

gevalCoreGeneralized' :: forall m ctxt c d . (EvaluationContext ctxt m, MonadUnliftIO m, MonadThrow m, MonadIO m) => ParserSpec ctxt -> ((Word32, ParsedRecord ctxt) -> c) -> (ConduitT c Void (ResourceT m) d) -> (d -> Double) -> (d -> Maybe GraphSeries) -> ctxt -> m (MetricOutput)
gevalCoreGeneralized' parserSpec itemStep aggregator finalStep generateGraph context = do
   v <- runResourceT $ runConduit $
     (((getZipSource $ (,)
       <$> ZipSource (CL.sourceList [(getFirstLineNo (Proxy :: Proxy m) context)..])
       <*> (ZipSource $ recordSource context parserSpec)) .| CL.map (checkStep (Proxy :: Proxy m) itemStep)) .| CL.catMaybes .| aggregator)
   return $ MetricOutput (SimpleRun $ finalStep v) (generateGraph v)


-- stuff for new dependent-type-based solution

gevalRunPipeline :: (EvaluationContext ctxt m, MonadUnliftIO m, MonadThrow m, MonadIO m)
                        => ParserSpec ctxt                   -- ^ parsers to parse data
                        -> (ParsedRecord ctxt -> c)          -- ^ function to go from the parsed value into
                                                             -- some "local" score calculated for each line (item)
                        -> ConduitT c Void (ResourceT m) MetricOutput
                        -> ctxt                              -- ^ "context", i.e. 2 or 3 sources needed to operate
                        -> m MetricOutput
gevalRunPipeline parserSpec itemStep finalPipeline context =
  gevalRunPipeline' parserSpec (skipLineNumber itemStep) finalPipeline context

gevalRunPipeline' :: forall m ctxt c . (EvaluationContext ctxt m, MonadUnliftIO m, MonadThrow m, MonadIO m) => ParserSpec ctxt -> ((Word32, ParsedRecord ctxt) -> c) -> ConduitT c Void (ResourceT m) MetricOutput -> ctxt -> m (MetricOutput)
gevalRunPipeline' parserSpec itemStep finalPipeline context = do
   runResourceT $ runConduit $
     (((getZipSource $ (,)
       <$> ZipSource (CL.sourceList [(getFirstLineNo (Proxy :: Proxy m) context)..])
       <*> (ZipSource $ recordSource context parserSpec)) .| CL.map (checkStep (Proxy :: Proxy m) itemStep)) .| CL.catMaybes .| finalPipeline)



continueGEvalCalculations :: (MonadIO m) =>
                            SAMetric t
                            -> Metric
                            -> ConduitT (ItemIntermediateRepresentationType t) Void (ResourceT m) MetricOutput

continueGEvalCalculations SAMultiLabelFMeasure (MultiLabelFMeasure beta) = defineContinuation countAgg (fMeasureOnCounts beta) noGraph

continueGEvalCalculations SALikelihood Likelihood = defineContinuation averageC logLossToLikehood noGraph

continueGEvalCalculations SAMultiLabelLikelihood MultiLabelLikelihood = defineContinuation averageC
                                                                                           logLossToLikehood
                                                                                           noGraph

continueGEvalCalculations SAMSE MSE = defineContinuation averageC id noGraph

continueGEvalCalculations SARMSE RMSE = defineContinuation averageC (** 0.5) noGraph

continueGEvalCalculations SAMAE MAE = defineContinuation averageC id noGraph

continueGEvalCalculations SASMAPE SMAPE = defineContinuation averageC (* 100.0) noGraph

continueGEvalCalculations SALogLoss LogLoss = defineContinuation averageC id noGraph

continueGEvalCalculations SABLEU BLEU = defineContinuation bleuAgg bleuFinal noGraph
  where bleuFinal (p1, p2, p3, p4, rl, l1, l2, l3, l4) = ((p1 /. l1) * (p2 /. l2) * (p3 /. l3) * (p4 /. l4)) ** 0.25 * (brevityPenalty l1 rl)
        bleuAgg = CC.foldl bleuFuse (0, 0, 0, 0, 0,  0, 0, 0, 0)
        bleuFuse (a1, a2, a3, a4, a5, a6, a7, a8, a9) (b1, b2, b3, b4, b5, b6, b7, b8, b9) = (a1+b1, a2+b2, a3+b3, a4+b4, a5+b5, a6+b6, a7+b7, a8+b8, a9+b9)
        brevityPenalty c r
          | c >= r = 1.0
          | c == 0 && r > 0 = 0.0
          | otherwise = exp (1.0 - (r /. c))

continueGEvalCalculations SAGLEU GLEU = defineContinuation gleuAgg gleuFinal noGraph
  where gleuFinal (m, t) = m /. t
        gleuAgg = CC.foldl gleuFuse (0, 0)
        gleuFuse (a1, a2) (b1, b2) = (a1+b1, a2+b2)

continueGEvalCalculations SAWER WER = defineContinuation werAgg werFinal noGraph
  where werAgg = CC.foldl werFuse (0, 0)
        werFuse (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)
        werFinal (errors, ref) = errors /. ref

continueGEvalCalculations SAAccuracy Accuracy = defineContinuation averageC id noGraph

continueGEvalCalculations SAFMeasure (FMeasure beta) = defineContinuation countAgg (fMeasureOnCounts beta) noGraph

continueGEvalCalculations SAMacroFMeasure (MacroFMeasure beta) = defineContinuation gatherClassC macroAverageOnCounts noGraph
                      where gatherClassC = CC.foldl gatherClassCombiner (M.empty, M.empty, M.empty)
                            gatherClassCombiner (tpMap, expectedMap, gotMap) (tp, expected, got) =
                              (insertMaybeToMap tp tpMap,
                               insertMaybeToMap expected expectedMap,
                               insertMaybeToMap got gotMap)
                            insertMaybeToMap Nothing m = m
                            insertMaybeToMap (Just c) m = M.insertWith (+) c 1 m
                            macroAverageOnCounts (tpMap, expectedMap, gotMap) =
                              (Prelude.sum
                               $ Prelude.map (\c -> fMeasureOnCounts beta (M.lookupDefault (0::Int) c tpMap,
                                                                         M.lookupDefault 0 c expectedMap,
                                                                         M.lookupDefault 0 c gotMap))
                               $ M.keys expectedMap) / (fromIntegral $ Prelude.length $ M.keys expectedMap)

continueGEvalCalculations SASoftFMeasure (SoftFMeasure beta) = defineContinuation countAgg
                                                                     (fMeasureOnCounts beta)
                                                                     noGraph

continueGEvalCalculations SAFLCFMeasure (FLCFMeasure beta) = defineContinuation countFragAgg
                                                                     (fMeasureOnFragCounts beta)
                                                                     noGraph

continueGEvalCalculations SASoft2DFMeasure (Soft2DFMeasure beta) = defineContinuation (CC.map (fMeasureOnCounts beta) .| averageC)
                                                                       id
                                                                       noGraph

continueGEvalCalculations SAClippEU ClippEU = defineContinuation clippeuAgg finalStep noGraph
  where
    clippeuAgg = CC.foldl countFolder (0, 0, 0)
    finalStep counts = f2MeasureOnCounts counts

continueGEvalCalculations SANMI NMI = defineContinuation (CC.foldl updateConfusionMatrix M.empty) normalizedMutualInformationFromConfusionMatrix noGraph

continueGEvalCalculations SAMAP MAP = defineContinuation averageC
                                                     id
                                                     noGraph

continueGEvalCalculations SABIOF1 BIOF1 = defineContinuation countAgg f1MeasureOnCounts noGraph

continueGEvalCalculations SABIOF1Labels BIOF1Labels = defineContinuation countAgg f1MeasureOnCounts noGraph

continueGEvalCalculations SASegmentAccuracy SegmentAccuracy = defineContinuation averageC id noGraph

continueGEvalCalculations SATokenAccuracy TokenAccuracy = defineContinuation hitsAndTotalsAgg
                                                               (\(hits, total) -> hits /. total)
                                                               noGraph
   where
         hitsAndTotalsAgg = CC.foldl (\(h1, t1) (h2, t2) -> (h1 + h2, t1 + t2)) (0, 0)

continueGEvalCalculations SAMultiLabelLogLoss MultiLabelLogLoss = defineContinuation averageC
                                                                   id
                                                                   noGraph


defineContinuation ::  (ConduitT c Void (ResourceT m) d)  -- ^ a Conduit which aggregates all the combined values into
                                                         -- a "total" value
                      -> (d -> Double)             -- ^ function to transform the "total" value into the final score
                      -> (d -> Maybe GraphSeries)
                      -> ConduitT c Void (ResourceT m) MetricOutput
defineContinuation aggregator finalStep generateGraph = do
  v <- aggregator
  return $ MetricOutput (SimpleRun $ finalStep v) (generateGraph v)

-- | A type family to handle all the evaluation "context".
--
-- This is needed as for some metrics the output and the expected metric is enough
-- (see the @WithoutInput@ instance), but for some the input is also needed to do
-- the evaluation (see the @WithInput@ instance).
class EvaluationContext ctxt m where
  data ParserSpec ctxt :: *
  data WrappedParsedRecord ctxt :: *
  data ParsedRecord ctxt :: *
  recordSource :: ctxt -> ParserSpec ctxt -> ConduitT () (WrappedParsedRecord ctxt) (ResourceT m) ()
  getFirstLineNo :: Proxy m -> ctxt -> Word32
  getExpectedSource :: ctxt -> SourceSpec
  getOutSource :: ctxt -> SourceSpec
  checkStep :: Proxy m -> ((Word32, ParsedRecord ctxt) -> c) -> (Word32, WrappedParsedRecord ctxt) -> Maybe c
  checkStepM :: ((Word32, ParsedRecord ctxt) -> (ResourceT m) c) -> (Word32, WrappedParsedRecord ctxt) -> (ResourceT m) (Maybe c)

data WithoutInput m e o = WithoutInput (LineSource (ResourceT m)) (LineSource (ResourceT m))

instance (MonadUnliftIO m, MonadIO m, MonadThrow m) => EvaluationContext (WithoutInput m e o) m where
  data ParserSpec (WithoutInput m e o) = ParserSpecWithoutInput (ItemTarget -> Either String e) (ItemTarget -> Either String o)
  data WrappedParsedRecord (WithoutInput m e o) = WrappedParsedRecordWithoutInput (SourceItem e) (SourceItem o)
  data ParsedRecord (WithoutInput m e o) = ParsedRecordWithoutInput e o
  getFirstLineNo _ (WithoutInput _ (LineSource _ _ _ _ lineNo)) = lineNo
  getExpectedSource (WithoutInput (LineSource _ _ _ expectedSource _) _) = expectedSource
  getOutSource (WithoutInput _ (LineSource _ _ _ outSource _)) = outSource
  recordSource (WithoutInput expectedLineSource outLineSource) (ParserSpecWithoutInput expParser outParser) = getZipSource $ WrappedParsedRecordWithoutInput
                        <$> ZipSource (items expectedLineSource expParser)
                        <*> ZipSource (items outLineSource outParser)
  checkStep _ step (lineNo, WrappedParsedRecordWithoutInput (Got expectedItem) (Got outItem)) = Just $ step (lineNo, ParsedRecordWithoutInput expectedItem outItem)
  checkStep _ _ (lineNo, WrappedParsedRecordWithoutInput _ (Wrong m)) = throw $ UnexpectedData lineNo m
  checkStep _ _ (lineNo, WrappedParsedRecordWithoutInput (Wrong m) _) = throw $ UnexpectedData lineNo m
  checkStep _ _ (_, WrappedParsedRecordWithoutInput (Got _) Done) = throw TooFewLines
  checkStep _ _ (_, WrappedParsedRecordWithoutInput Done (Got _)) = throw TooManyLines
  checkStep _ _ (_, WrappedParsedRecordWithoutInput Done Done) = Nothing

  checkStepM step (lineNo, WrappedParsedRecordWithoutInput (Got expectedItem) (Got outItem)) = Just <$> step (lineNo, ParsedRecordWithoutInput expectedItem outItem)
  checkStepM _ (lineNo, WrappedParsedRecordWithoutInput _ (Wrong m)) = throw $ UnexpectedData lineNo m
  checkStepM _ (lineNo, WrappedParsedRecordWithoutInput (Wrong m) _) = throw $ UnexpectedData lineNo m
  checkStepM _ (_, WrappedParsedRecordWithoutInput (Got _) Done) = throwM TooFewLines
  checkStepM _ (_, WrappedParsedRecordWithoutInput Done (Got _)) = throwM TooManyLines
  checkStepM _ (_, WrappedParsedRecordWithoutInput Done Done) = return Nothing


data WithInput m i e o = WithInput (LineSource (ResourceT m)) (LineSource (ResourceT m)) (LineSource (ResourceT m))

getInputFilePath (WithInput (LineSource _ _ _ inputFilePath _) _ _) = inputFilePath

instance (MonadUnliftIO m, MonadIO m, MonadThrow m) => EvaluationContext (WithInput m i e o) m where
  data ParserSpec (WithInput m i e o) = ParserSpecWithInput (ItemTarget -> Either String i) (ItemTarget -> Either String e) (ItemTarget -> Either String o)
  data WrappedParsedRecord (WithInput m i e o) = WrappedParsedRecordWithInput (SourceItem i) (SourceItem e) (SourceItem o)
  data ParsedRecord (WithInput m i e o) = ParsedRecordWithInput i e o
  getFirstLineNo _ (WithInput _ _ (LineSource _ _ _ _ lineNo)) = lineNo
  getExpectedSource (WithInput _ (LineSource _ _ _ expectedSource _) _) = expectedSource
  getOutSource (WithInput _ _ (LineSource _ _ _ outSource _)) = outSource
  recordSource (WithInput inputLineSource expectedLineSource outLineSource) (ParserSpecWithInput inpParser expParser outParser) = getZipSource $ (\x (y,z) -> WrappedParsedRecordWithInput x y z)
         <$> ZipSource (items inputLineSource inpParser)                                                                         <*> (ZipSource $ getZipSource $ (,)
                        <$> ZipSource (items expectedLineSource expParser)
                        <*> ZipSource (items outLineSource outParser))
  checkStep _ step (lineNo, WrappedParsedRecordWithInput (Got inputItem) (Got expectedItem) (Got outItem)) = Just $ step (lineNo, ParsedRecordWithInput inputItem expectedItem outItem)
  checkStep _ _ (lineNo, WrappedParsedRecordWithInput _ _ (Wrong m)) = throw $ UnexpectedData lineNo m
  checkStep _ _ (lineNo, WrappedParsedRecordWithInput _ (Wrong m) _) = throw $ UnexpectedData lineNo m
  checkStep _ _ (lineNo, WrappedParsedRecordWithInput (Wrong m) _ _) = throw $ UnexpectedData lineNo m
  checkStep _ _ (_, WrappedParsedRecordWithInput _ (Got _) Done) = throw TooFewLines
  checkStep _ _ (_, WrappedParsedRecordWithInput _ Done (Got _)) = throw TooManyLines
  checkStep _ _ (_, WrappedParsedRecordWithInput Done (Got _) (Got _)) = throw TooFewLinesInInput
  checkStep _ _ (_, WrappedParsedRecordWithInput (Got _) Done Done) = throw TooManyLinesInInput
  checkStep _ _ (_, WrappedParsedRecordWithInput Done Done Done) = Nothing

  checkStepM step (lineNo, WrappedParsedRecordWithInput (Got inputItem) (Got expectedItem) (Got outItem)) = Just <$> step (lineNo, ParsedRecordWithInput inputItem expectedItem outItem)
  checkStepM _ (lineNo, WrappedParsedRecordWithInput _ _ (Wrong m)) = throw $ UnexpectedData lineNo m
  checkStepM _ (lineNo, WrappedParsedRecordWithInput _ (Wrong m) _) = throw $ UnexpectedData lineNo m
  checkStepM _ (lineNo, WrappedParsedRecordWithInput (Wrong m) _ _) = throw $ UnexpectedData lineNo m
  checkStepM _ (_, WrappedParsedRecordWithInput _ (Got _) Done) = throw TooFewLines
  checkStepM _ (_, WrappedParsedRecordWithInput _ Done (Got _)) = throw TooManyLines
  checkStepM _ (_, WrappedParsedRecordWithInput Done (Got _) (Got _)) = throw TooFewLinesInInput
  checkStepM _ (_, WrappedParsedRecordWithInput (Got _) Done Done) = throw TooManyLinesInInput
  checkStepM _ (_, WrappedParsedRecordWithInput Done Done Done) = return Nothing


threeLineSource :: (MonadUnliftIO m, MonadIO m, MonadThrow m) => WithInput m Text Text Text -> ConduitT () (WrappedParsedRecord (WithInput m Text Text Text)) (ResourceT m) ()
threeLineSource (WithInput inputLineSource expectedLineSource outLineSource) = getZipSource $ (\x (y,z) -> WrappedParsedRecordWithInput x y z)
         <$> ZipSource (linesAsItems inputLineSource)                                             <*> (ZipSource $ getZipSource $ (,)
                        <$> ZipSource (linesAsItems expectedLineSource)
                        <*> ZipSource (linesAsItems outLineSource))

averageC :: MonadResource m => ConduitT Double Void m Double
averageC = getZipSink
    $ (\total count -> total / fromIntegral count)
  <$> ZipSink CC.sum
  <*> ZipSink CC.length

-- | Takes a source of lines and returns a source of lines and returns a conduit of
-- items (using a given preprocessor and parser).
items :: MonadResource m => LineSource m -> (ItemTarget -> Either String a) -> ConduitT () (SourceItem a) m ()
items (LineSource lineSource itemDecoder preprocess _ _) parser =
  (lineSource .| CL.map (toItem . parser . preprocess' . itemDecoder)) >> yield Done
  where toItem (Right x) = Got x
        toItem (Left m) = Wrong m
        preprocess' (RawItemTarget t) = RawItemTarget $ preprocess t
        preprocess' (PartiallyParsedItemTarget ts) = PartiallyParsedItemTarget $ Prelude.map preprocess ts

-- | Takes a source of lines and returns a conduit of lines represented as
-- items (without preprocessing and parsing!) to be used in line-by-line modes.
linesAsItems :: MonadResource m => LineSource m -> ConduitT () (SourceItem Text) m ()
linesAsItems (LineSource lineSource _ _ _ _) =
  (lineSource .| CL.map Got) >> yield Done
