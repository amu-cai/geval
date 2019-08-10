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
      GEvalException(..),
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
      gesPreprocess,
      getDataDecoder,
      threeLineSource,
      extensionsHandled,
      isEmptyFile,
      somethingWrongWithFilesMessage
    ) where

import GEval.Metric

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
import Text.Read (readMaybe)
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

import qualified Data.IntSet as IS

import GEval.BLEU
import GEval.Common
import GEval.ClippEU
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
                            gesExpectedFile :: String,
                            gesInputFile :: String,
                            gesMetrics :: [Metric],
                            gesPrecision :: Maybe Int,
                            gesTokenizer :: Maybe Tokenizer,
                            gesGonitoHost :: Maybe String,
                            gesToken :: Maybe String,
                            gesGonitoGitAnnexRemote :: Maybe String,
                            gesReferences :: Maybe String }


gesMainMetric :: GEvalSpecification -> Metric
gesMainMetric spec = case gesMetrics spec of
  (metric:_) -> metric
  otherwise -> error "no metric given"

gesPreprocess :: GEvalSpecification -> (Text -> Text)
gesPreprocess spec = tokenizeTabSeparatedWithSpaces (gesTokenizer spec)

getExpectedDirectory :: GEvalSpecification -> FilePath
getExpectedDirectory spec = fromMaybe outDirectory $ gesExpectedDirectory spec
                            where outDirectory = gesOutDirectory spec

-- | Special command, not just running the regular evaluation.
-- See OptionsParser.hs for more information.
data GEvalSpecialCommand = Init
                           | LineByLine | WorstFeatures
                           | Diff FilePath | MostWorseningFeatures FilePath
                           | PrintVersion | JustTokenize | Submit
                           | Validate

data ResultOrdering = KeepTheOriginalOrder | FirstTheWorst | FirstTheBest

data GEvalOptions = GEvalOptions
                    { geoSpecialCommand :: Maybe GEvalSpecialCommand,
                      geoResultOrdering :: ResultOrdering,
                      geoFilter :: Maybe String,
                      geoSpec :: GEvalSpecification,
                      geoBlackBoxDebugginsOptions :: BlackBoxDebuggingOptions,
                      geoGraphFile :: Maybe FilePath }

data GEvalException = NoExpectedFile FilePath
                      | NoOutFile FilePath
                      | NoExpectedDirectory FilePath
                      | NoOutDirectory FilePath
                      | NoExpectedTestDirectory FilePath
                      | NoOutTestDirectory FilePath
                      | NoInputFile FilePath
                      | FileAlreadyThere FilePath
                      | TooFewLines
                      | TooManyLines
                      | TooFewLinesInInput
                      | TooManyLinesInInput
                      | EmptyOutput
                      | UnexpectedData Word32 String
                      | UnexpectedMultipleOutputs
                      | OtherException String
                      deriving (Eq)

instance Exception GEvalException

instance Show GEvalException where
  show (NoExpectedFile filePath) = somethingWrongWithFilesMessage "No file with the expected results" filePath
  show (NoOutFile filePath) = somethingWrongWithFilesMessage "No file with the test results" filePath
  show (NoExpectedDirectory filePath) = somethingWrongWithFilesMessage "No directory with the expected results" filePath
  show (NoOutDirectory filePath) = somethingWrongWithFilesMessage "No directory with the test results" filePath
  show (NoExpectedTestDirectory filePath) = somethingWrongWithFilesMessage "No test subdirectory with the expected results" filePath
  show (NoOutTestDirectory filePath) = somethingWrongWithFilesMessage "No test subdirectory with the results obtained" filePath
  show (NoInputFile filePath) = somethingWrongWithFilesMessage "No file with the input" filePath
  show (FileAlreadyThere filePath) = somethingWrongWithFilesMessage "File already there" filePath
  show TooFewLines = "Too few lines in the output file"
  show TooManyLines = "Too many lines in the output file"
  show TooFewLinesInInput = "Too few lines in the input file"
  show TooManyLinesInInput = "Too many lines in the input file"
  show EmptyOutput = "The output file is empty"
  show (UnexpectedData lineNo message) = "Line " ++ (show lineNo) ++ ": Unexpected data [" ++ message ++ "]"
  show UnexpectedMultipleOutputs = "Multiple outputs are not possible in this mode, use -o option to select an output file"
  show (OtherException message) = message

somethingWrongWithFilesMessage :: String -> FilePath -> String
somethingWrongWithFilesMessage msg filePath = Prelude.concat
                                [ msg, ": `", filePath, "`" ]

defaultGEvalSpecification = GEvalSpecification {
  gesOutDirectory = defaultOutDirectory,
  gesExpectedDirectory = Nothing,
  gesTestName = defaultTestName,
  gesSelector = Nothing,
  gesOutFile = defaultOutFile,
  gesExpectedFile = defaultExpectedFile,
  gesInputFile = defaultInputFile,
  gesMetrics = [defaultMetric],
  gesPrecision = Nothing,
  gesTokenizer = Nothing,
  gesGonitoHost = Nothing,
  gesToken = Nothing,
  gesGonitoGitAnnexRemote = Nothing,
  gesReferences = Nothing}

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
  (inputSource, expectedSource, outSources) <- checkAndGetFiles False gevalSpec
  results <- Prelude.mapM (gevalOnSingleOut gevalSpec inputSource expectedSource) outSources
  return $ sortBy (\a b ->  (show $ fst a) `naturalComp` (show $ fst b)) results

noGraph :: d -> Maybe GraphSeries
noGraph = const Nothing

gevalOnSingleOut :: GEvalSpecification -> SourceSpec -> SourceSpec -> SourceSpec -> IO (SourceSpec, [MetricOutput])
gevalOnSingleOut gevalSpec inputSource expectedSource outSource = do
  vals <- Prelude.mapM (\metric -> gevalCore metric mSelector preprocess inputSource expectedSource outSource) metrics
  return (outSource, vals)
  where metrics = gesMetrics gevalSpec
        preprocess = gesPreprocess gevalSpec
        mSelector = gesSelector gevalSpec

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
       inputSource <- getInputSourceIfNeeded forceInput metrics expectedTestDirectory inputFile

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
          metrics = gesMetrics gevalSpec

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

fileAsLineSource :: SourceSpec -> Maybe Selector -> (Text -> Text) -> LineSource (ResourceT IO)
fileAsLineSource spec mSelector preprocess =
  LineSource ((smartSource spec) .| autoDecompress .| CT.decodeUtf8Lenient .| CT.lines) (select (getDataFormat spec) mSelector) preprocess spec 1

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

-- | Runs evaluation for a given metric using the sources specified
-- for input, expected output and output. Returns the metric value.
-- Throws @GEvalException@ if something was wrong in the data (e.g.
-- inconsistent number of lines in the sources).
gevalCore :: Metric           -- ^ evaluation metric
          -> Maybe Selector   -- ^ selector to be used
          -> (Text -> Text)    -- ^ preprocessing function (e.g. tokenization)
          -> SourceSpec       -- ^ source specification for the input values
          -> SourceSpec       -- ^ source specification for the expected output
          -> SourceSpec       -- ^ source specification for the output
          -> IO (MetricOutput) -- ^ metric value for the output against the expected output
gevalCore metric mSelector preprocess inputSource expectedSource outSource = do
  whenM (isEmptyFileSource outSource) $ throwM $ EmptyOutput
  gevalCoreOnSources metric
                     (fileAsLineSource inputSource mSelector preprocess)
                     (fileAsLineSource expectedSource mSelector preprocess)
                     (fileAsLineSource outSource mSelector preprocess)

isEmptyFileSource :: SourceSpec -> IO Bool
isEmptyFileSource (FilePathSpec filePath) = isEmptyFile filePath
isEmptyFileSource _ = return False

logLossToLikehood logLoss = exp (-logLoss)

-- | Runs evaluation for a given metric using the sources given
-- for input, expected output and output. Returns the metric value.
-- Throws @GEvalException@ if something was wrong in the data (e.g.
-- inconsistent number of lines in the sources).
--
-- The difference between this and @gevalCore@ is that it operates on Conduit
-- sources (rather than source specification).
--
-- This could be specialised for particular metrics, if they could be
-- calculated from other metrics in a trivial fashion (e.g. @RMSE@
-- which is just a square root of @MSE@). Otherwise a metric should be
-- defined in @gevalCore'@ and @gevalCoreWithoutInput@ helper
-- functions.
gevalCoreOnSources :: (MonadIO m, MonadThrow m, MonadUnliftIO m) =>
                     Metric                      -- ^ evaluation metric
                     -> LineSource (ResourceT m)  -- ^ source of the input values
                     -> LineSource (ResourceT m)  -- ^ source to read the expected output
                     -> LineSource (ResourceT m)  -- ^ source to read the output
                     -> m (MetricOutput)           -- ^ metric values for the output against the expected output
gevalCoreOnSources RMSE inputLineSource expectedLineSource outLineSource = do
  MetricOutput mse g <- gevalCoreOnSources MSE inputLineSource expectedLineSource outLineSource
  return $ MetricOutput (mse ** 0.5) g

gevalCoreOnSources Likelihood inputLineSource expectedLineSource outLineSource = do
  MetricOutput logLoss g <- gevalCoreOnSources LogLoss inputLineSource expectedLineSource outLineSource
  return $ MetricOutput (logLossToLikehood logLoss) g

gevalCoreOnSources (LikelihoodHashed b) inputLineSource expectedLineSource outLineSource = do
  MetricOutput logLoss g <- gevalCoreOnSources (LogLossHashed b) inputLineSource expectedLineSource outLineSource
  return $ MetricOutput (logLossToLikehood logLoss) g

gevalCoreOnSources MultiLabelLikelihood inputLineSource expectedLineSource outLineSource = do
  MetricOutput logLoss g <- gevalCoreOnSources MultiLabelLogLoss inputLineSource expectedLineSource outLineSource
  return $ MetricOutput (logLossToLikehood logLoss) g

gevalCoreOnSources metric inputLineSource expectedLineSource outLineSource = do
  gevalCore' metric inputLineSource expectedLineSource outLineSource

data LineInFile = LineInFile SourceSpec Word32 Text

-- | Runs evaluation for a given metric using the sources given
-- for input, expected output and output. Returns the metric value.
-- Throws @GEvalException@ if something was wrong in the data (e.g.
-- inconsistent number of lines in the sources).
--
-- Metrics are starting to be really defined here, though when the
-- input is not needed for doing the evaluation (which is not in most
-- cases), the work is delegated to @gevalCoreWithoutInput@ function.
gevalCore' :: (MonadIO m, MonadThrow m, MonadUnliftIO m) =>
           Metric                      -- ^ evaluation metric
           -> LineSource (ResourceT m)  -- ^ source of the input values
           -> LineSource (ResourceT m)  -- ^ source to read the expected output
           -> LineSource (ResourceT m)  -- ^ source to read the output
           -> m (MetricOutput)           -- ^ metric values for the output against the expected output
gevalCore' MSE _ = gevalCoreWithoutInput outParser outParser itemSquaredError averageC id noGraph
  where outParser = getValue . TR.double

gevalCore' MAE _ = gevalCoreWithoutInput outParser outParser itemAbsoluteError averageC id noGraph
  where outParser = getValue . TR.double

gevalCore' SMAPE _ = gevalCoreWithoutInput outParser outParser smape averageC (* 100.0) noGraph
  where outParser = getValue . TR.double
        smape (exp, out) = (abs (exp-out)) `safeDoubleDiv` ((abs exp) + (abs out))

gevalCore' Pearson _ = gevalCoreByCorrelationMeasure pearson
gevalCore' Spearman _ = gevalCoreByCorrelationMeasure spearman

gevalCore' LogLoss _ = gevalCoreWithoutInput outParser outParser itemLogLossError averageC id noGraph
  where outParser = getValue . TR.double

gevalCore' BLEU _ = gevalCoreWithoutInput (Right . Prelude.map Prelude.words . DLS.splitOn "\t" . unpack) (Right . Prelude.words . unpack) bleuCombine bleuAgg bleuFinal noGraph
  where bleuFinal (p1, p2, p3, p4, rl, l1, l2, l3, l4) = ((p1 /. l1) * (p2 /. l2) * (p3 /. l3) * (p4 /. l4)) ** 0.25 * (brevityPenalty l1 rl)
        bleuCombine (refs, sen) = bleuStep refs sen
        bleuAgg = CC.foldl bleuFuse (0, 0, 0, 0, 0,  0, 0, 0, 0)
        bleuFuse (a1, a2, a3, a4, a5, a6, a7, a8, a9) (b1, b2, b3, b4, b5, b6, b7, b8, b9) = (a1+b1, a2+b2, a3+b3, a4+b4, a5+b5, a6+b6, a7+b7, a8+b8, a9+b9)
        brevityPenalty c r
          | c >= r = 1.0
          | c == 0 && r > 0 = 0.0
          | otherwise = exp (1.0 - (r /. c))

gevalCore' GLEU _ = gevalCoreWithoutInput (Right . Prelude.map Prelude.words . DLS.splitOn "\t" . unpack) (Right . Prelude.words . unpack) gleuCombine gleuAgg gleuFinal noGraph
  where gleuFinal (m, t) = m /. t
        gleuCombine (refs, sen) = gleuStep refs sen
        gleuAgg = CC.foldl gleuFuse (0, 0)
        gleuFuse (a1, a2) (b1, b2) = (a1+b1, a2+b2)

gevalCore' WER _ = gevalCoreWithoutInput (Right . Prelude.words . unpack) (Right . Prelude.words . unpack) (uncurry werStep) averageC id noGraph

gevalCore' Accuracy _ = gevalCoreWithoutInput (Right . strip) (Right . strip) hitOrMiss averageC id noGraph
                      where hitOrMiss (exp, got) =
                              -- first try to parse what we got as a probability distribution
                              -- (like the one used for Likelikehood/LogLossHashed metric)
                              case parseWordSpecs got of
                                Right wordSpecs -> if Prelude.null pairs
                                                   then 0.0
                                                   else indicator (exp == (snd $ Prelude.maximum pairs))
                                                 where pairs = catMaybes $ Prelude.map wordSpecToPair wordSpecs
                                Left _ ->  indicator ((normalizeProbForAccuracy exp got) == exp)
                                          -- if the expected value is 0 or 1 treat values
                                          -- between 0.0 and 1.0 as probabilities
                                          -- for the positive outcome
                            normalizeProbForAccuracy :: Text -> Text -> Text
                            normalizeProbForAccuracy exp got
                              | exp == (pack "1") = case tryReadingAsFloat got of
                                            Just p -> if p >= 0.5 && p <= 1.0 then exp else got
                                            Nothing -> got
                              | exp == (pack "0") = case tryReadingAsFloat got of
                                            Just p -> if p < 0.5 && p >= 0.0 then exp else got
                                            Nothing -> got
                              | otherwise = got
                            tryReadingAsFloat :: Text -> Maybe Float
                            tryReadingAsFloat = readMaybe . unpack

gevalCore' (FMeasure beta) _ = gevalCoreWithoutInput outParser outParser getCount countAgg (fMeasureOnCounts beta) noGraph
  where outParser = detected <=< (getValue . TR.double)
        expParser = expected <=< (getValue . TR.decimal)
        expected 1 = Right True
        expected 0 = Right False
        expected _ = Left "expected 0 or 1"
        -- output value could be a probability (for compatibility with other measures)
        detected prob
          | prob >= 0.0 && prob < detectionThreshold = Right False
          | prob >= detectionThreshold && prob <= 1.0 = Right True
          | otherwise = Left "expected probability"
        detectionThreshold = 0.5
        getCount :: (Bool, Bool) -> (Int, Int, Int)
        getCount (True, True)   = (1, 1, 1)
        getCount (True, False)  = (0, 1, 0)
        getCount (False, True)  = (0, 0, 1)
        getCount (False, False) = (0, 0, 0)

gevalCore' (MacroFMeasure beta) _ = gevalCoreWithoutInput (Right . Just . strip) (Right . predicted . strip) getClassesInvolved gatherClassC macroAverageOnCounts noGraph
                      where predicted got =
                              -- first try to parse what we got as a probability distribution
                              -- (like the one used for Likelikehood/LogLossHashed metric)
                              case parseWordSpecs got of
                                Right wordSpecs -> if Prelude.null pairs
                                                   then Nothing
                                                   else Just $ snd $ Prelude.maximum pairs
                                                 where pairs = catMaybes $ Prelude.map wordSpecToPair wordSpecs
                                Left _ -> Just got
                            getClassesInvolved (Just a, Nothing) = (Nothing, Just a, Nothing)
                            getClassesInvolved (Nothing, Just b) = (Nothing, Nothing, Just b) -- should not occur, for completeness
                            getClassesInvolved (Just a, Just b) = if a == b
                                                                     then (Just a, Just a, Just a)
                                                                     else (Nothing, Just a, Just b)
                            gatherClassC = CC.foldl gatherClassCombiner (M.empty, M.empty, M.empty)
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

gevalCore' (SoftFMeasure beta) _ = gevalCoreWithoutInput parseAnnotations
                                                         parseObtainedAnnotations
                                                         getSoftCounts
                                                         countAgg
                                                         (fMeasureOnCounts beta)
                                                         noGraph
                      where getSoftCounts (expected, got) = (weightedMaxMatch matchScore expected got,
                                                             Prelude.length expected,
                                                             Prelude.length got)

gevalCore' (ProbabilisticSoftFMeasure beta) _ = gevalCoreWithoutInput parseAnnotations
                                                                      parseObtainedAnnotations
                                                                      getProbabilisticSoftCounts
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

gevalCore' ClippEU _ = gevalCoreWithoutInput parseClippingSpecs parseClippings matchStep clippeuAgg finalStep noGraph
  where
    parseClippings = controlledParse lineClippingsParser
    parseClippingSpecs = controlledParse lineClippingSpecsParser
    matchStep (clippingSpecs, clippings) = (maxMatch matchClippingToSpec clippingSpecs clippings,
                                            Prelude.length clippingSpecs,
                                            Prelude.length clippings)
    clippeuAgg = CC.foldl countFolder (0, 0, 0)
    finalStep counts = f2MeasureOnCounts counts

gevalCore' NMI _ = gevalCoreWithoutInput (Right . id) (Right . id) id (CC.foldl updateConfusionMatrix M.empty) normalizedMutualInformationFromConfusionMatrix noGraph

gevalCore' MAP _ = gevalCoreWithoutInput (Right . DLS.splitOn "\t" . unpack)
                                         (Right . DLS.splitOn "\t" . unpack)
                                         (\(e,g) -> calculateMAPForOneResult e g)
                                         averageC
                                         id
                                         noGraph

gevalCore' (LogLossHashed nbOfBits) _ = helper nbOfBits
  -- for LogLossHashed we "salt" each hash with the line number
  where helper nbOfBits expectedLineSource outLineSource =
          gevalCore''' (ParserSpecWithoutInput (liftOp (Right . id)) (liftOp tentativeParser)) (\(lineNo, (t,d)) -> calculateLogLoss nbOfBits lineNo t (parseDistributionWrapper nbOfBits lineNo d)) averageC negate noGraph (WithoutInput expectedLineSource outLineSource)
        -- Unfortunately, we're parsing the distribution twice. We need to
        -- tentatively parse the distribution when the line number is unknown
        -- (so we just set it to 1)
        -- @TODO Fix this.
        tentativeParser t = case parseDistribution nbOfBits 1 t of
          Right _ -> Right t
          Left m -> Left m

gevalCore' CharMatch inputLineSource = helper inputLineSource
 where
   helper inputLineSource expectedLineSource outputLineSource = do
     gevalCoreGeneralized (ParserSpecWithInput justUnpack justUnpack justUnpack) step countAgg (fMeasureOnCounts charMatchBeta) noGraph (WithInput inputLineSource expectedLineSource outputLineSource)
   step (ParsedRecordWithInput inp exp out) = getCharMatchCount inp exp out
   justUnpack = liftOp (Right . unpack)


gevalCore' BIOF1 _ = gevalCoreWithoutInput parseBioSequenceIntoEntities parseBioSequenceIntoEntities (uncurry gatherCountsForBIO) countAgg f1MeasureOnCounts noGraph

gevalCore' BIOF1Labels _ = gevalCoreWithoutInput parseBioSequenceIntoEntitiesWithoutNormalization parseBioSequenceIntoEntitiesWithoutNormalization (uncurry gatherCountsForBIO) countAgg f1MeasureOnCounts noGraph
   where parseBioSequenceIntoEntitiesWithoutNormalization s = do
           entities <- parseBioSequenceIntoEntities s
           return $ Prelude.map eraseNormalisation entities

gevalCore' TokenAccuracy _ = gevalCoreWithoutInput intoTokens
                                                   intoTokens
                                                   countHitsAndTotals
                                                   hitsAndTotalsAgg
                                                   (\(hits, total) -> hits /. total)
                                                   noGraph
   where intoTokens = Right . Data.Text.words
         countHitsAndTotals :: ([Text], [Text]) -> (Int, Int)
         countHitsAndTotals (es, os) =
             if Prelude.length os /= Prelude.length es
               then throw $ OtherException "wrong number of tokens"
               else Prelude.foldl matchFun
                                  (0, 0)
                                  (Prelude.zip es os)
         matchFun :: (Int, Int) -> (Text, Text) -> (Int, Int)
         matchFun (h, t) (e, o)
           | e == (pack "*") = (h, t)
           | o `Prelude.elem` (splitOn (pack ";") e) = (h + 1, t + 1)
           | otherwise = (h, t + 1)
         hitsAndTotalsAgg = CC.foldl (\(h1, t1) (h2, t2) -> (h1 + h2, t1 + t2)) (0, 0)

-- only MultiLabel-F1 handled for JSONs for the time being...
gevalCore' (MultiLabelFMeasure beta) _ = gevalCoreWithoutInputOnItemTargets (Right . intoWords)
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

gevalCore' MultiLabelLogLoss _ = gevalCoreWithoutInput intoWords
                                                       (Right . parseIntoProbList)
                                                       (uncurry countLogLossOnProbList)
                                                       averageC
                                                       id
                                                       noGraph
    where
      intoWords = Right . Data.Text.words

countAgg :: (Num n, Monad m) => ConduitM (n, Int, Int) o m (n, Int, Int)
countAgg = CC.foldl countFolder (fromInteger 0, 0, 0)

gevalCoreByCorrelationMeasure :: (MonadUnliftIO m, MonadThrow m, MonadIO m) =>
                                (V.Vector (Double, Double) -> Double) -> -- ^ correlation function
                                LineSource (ResourceT m) ->  -- ^ source to read the expected output
                                LineSource (ResourceT m) ->  -- ^ source to read the output
                                m (MetricOutput)             -- ^ metric values for the output against the expected output
gevalCoreByCorrelationMeasure correlationFunction =
  gevalCoreWithoutInput outParser outParser id correlationC finalStep noGraph
  where outParser = getValue . TR.double
        correlationC = CC.foldl (flip (:)) []
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
                      => (Text -> Either String a) -- ^ parser for values in the expected output
                      -> (Text -> Either String b) -- ^ parser for values in the actual output
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
gevalCoreWithoutInput expParser outParser itemStep aggregator finalStep generateGraph expectedLineStream outLineStream =
  gevalCoreWithoutInputOnItemTargets (liftOp expParser) (liftOp outParser) itemStep aggregator finalStep generateGraph expectedLineStream outLineStream

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
   return $ MetricOutput (finalStep v) (generateGraph v)

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

itemAbsoluteError :: (Double, Double) -> Double
itemAbsoluteError (exp, out) = abs (exp-out)

itemSquaredError :: (Double, Double) -> Double
itemSquaredError (exp, out) = (exp-out)**2


itemLogLossError :: (Double, Double) -> Double
itemLogLossError (exp, out)
  | exp' > 0.5 = - (log out')
  | otherwise = - (log (1 - out'))
  where exp' = normalizeAsProb exp
        out' = normalizeAsProb out
        normalizeAsProb v
          | v >= 1.0 = 1.0
          | v <= 0.0 = 0.0
          | otherwise = v

getValue :: Num a => Either String (a, Text) -> Either String a
getValue (Right (x, reminder)) =
  if Data.Text.null reminder || Data.Text.head reminder == '\t'
  then Right x
  else Left "number expected"
getValue (Left s) = Left s

controlledParse parser t =
  case parseOnly parser t of
    (Right v) -> Right v
    (Left _) -> Left "cannot parse line"
