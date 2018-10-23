{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module GEval.Core
    ( geval,
      gevalCore,
      gevalCoreOnSingleLines,
      LineInFile(..),
      Metric(..),
      MetricOrdering(..),
      getMetricOrdering,
      MetricValue,
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
      gesPreprocess
    ) where

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
import Data.Tuple
import qualified Data.List.Split as DLS
import Data.List (sortBy)
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
import GEval.Annotation

import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

import Statistics.Correlation

import Data.Proxy

import Data.Word

import System.FilePath.Glob

type MetricValue = Double

defaultLogLossHashedSize :: Word32
defaultLogLossHashedSize = 10

-- | evaluation metric
data Metric = RMSE | MSE | Pearson | Spearman | BLEU | GLEU | WER | Accuracy | ClippEU
              | FMeasure Double | MacroFMeasure Double | NMI
              | LogLossHashed Word32 | CharMatch | MAP | LogLoss | Likelihood
              | BIOF1 | BIOF1Labels | LikelihoodHashed Word32 | MAE | MultiLabelFMeasure Double
              | MultiLabelLogLoss | MultiLabelLikelihood
              | SoftFMeasure Double
              deriving (Eq)

instance Show Metric where
  show RMSE = "RMSE"
  show MSE  = "MSE"
  show Pearson = "Pearson"
  show Spearman = "Spearman"
  show BLEU = "BLEU"
  show GLEU = "GLEU"
  show WER = "WER"
  show Accuracy = "Accuracy"
  show ClippEU = "ClippEU"
  show (FMeasure beta) = "F" ++ (show beta)
  show (MacroFMeasure beta) = "Macro-F" ++ (show beta)
  show (SoftFMeasure beta) = "Soft-F" ++ (show beta)
  show NMI = "NMI"
  show (LogLossHashed nbOfBits) = "LogLossHashed" ++ (if
                                                       nbOfBits == defaultLogLossHashedSize
                                                      then
                                                       ""
                                                      else
                                                       (show nbOfBits))
  show (LikelihoodHashed nbOfBits) = "LikelihoodHashed" ++ (if
                                                               nbOfBits == defaultLogLossHashedSize
                                                            then
                                                              ""
                                                            else
                                                              (show nbOfBits))
  show CharMatch = "CharMatch"
  show MAP = "MAP"
  show LogLoss = "LogLoss"
  show Likelihood = "Likelihood"
  show BIOF1 = "BIO-F1"
  show BIOF1Labels = "BIO-F1-Labels"
  show MAE = "MAE"
  show (MultiLabelFMeasure beta) = "MultiLabel-F" ++ (show beta)
  show MultiLabelLogLoss = "MultiLabel-Logloss"
  show MultiLabelLikelihood = "MultiLabel-Likelihood"

instance Read Metric where
  readsPrec _ ('R':'M':'S':'E':theRest) = [(RMSE, theRest)]
  readsPrec _ ('M':'S':'E':theRest) = [(MSE, theRest)]
  readsPrec _ ('P':'e':'a':'r':'s':'o':'n':theRest) = [(Pearson, theRest)]
  readsPrec _ ('S':'p':'e':'a':'r':'m':'a':'n':theRest) = [(Spearman, theRest)]
  readsPrec _ ('B':'L':'E':'U':theRest) = [(BLEU, theRest)]
  readsPrec _ ('G':'L':'E':'U':theRest) = [(GLEU, theRest)]
  readsPrec _ ('W':'E':'R':theRest) = [(WER, theRest)]
  readsPrec _ ('A':'c':'c':'u':'r':'a':'c':'y':theRest) = [(Accuracy, theRest)]
  readsPrec _ ('C':'l':'i':'p':'p':'E':'U':theRest) = [(ClippEU, theRest)]
  readsPrec _ ('N':'M':'I':theRest) = [(NMI, theRest)]
  readsPrec p ('F':theRest) = case readsPrec p theRest of
    [(beta, theRest)] -> [(FMeasure beta, theRest)]
    _ -> []
  readsPrec p ('M':'a':'c':'r':'o':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest)] -> [(MacroFMeasure beta, theRest)]
    _ -> []
  readsPrec p ('M':'u':'l':'t':'i':'L':'a':'b':'e':'l':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest)] -> [(MultiLabelFMeasure beta, theRest)]
    _ -> []
  readsPrec p ('S':'o':'f':'t':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest)] -> [(SoftFMeasure beta, theRest)]
    _ -> []
  readsPrec p ('L':'o':'g':'L':'o':'s':'s':'H':'a':'s':'h':'e':'d':theRest) = case readsPrec p theRest of
    [(nbOfBits, theRest)] -> [(LogLossHashed nbOfBits, theRest)]
    _ -> [(LogLossHashed defaultLogLossHashedSize, theRest)]
  readsPrec p ('L':'i':'k':'e':'l':'i':'h':'o':'o':'d':'H':'a':'s':'h':'e':'d':theRest) = case readsPrec p theRest of
    [(nbOfBits, theRest)] -> [(LikelihoodHashed nbOfBits, theRest)]
    _ -> [(LikelihoodHashed defaultLogLossHashedSize, theRest)]
  readsPrec _ ('L':'o':'g':'L':'o':'s':'s':theRest) = [(LogLoss, theRest)]
  readsPrec _ ('L':'i':'k':'e':'l':'i':'h':'o':'o':'d':theRest) = [(Likelihood, theRest)]
  readsPrec p ('C':'h':'a':'r':'M':'a':'t':'c':'h':theRest) = [(CharMatch, theRest)]
  readsPrec _ ('M':'A':'P':theRest) = [(MAP, theRest)]
  readsPrec _ ('B':'I':'O':'-':'F':'1':'-':'L':'a':'b':'e':'l':'s':theRest) = [(BIOF1Labels, theRest)]
  readsPrec _ ('B':'I':'O':'-':'F':'1':theRest) = [(BIOF1, theRest)]
  readsPrec _ ('M':'A':'E':theRest) = [(MAE, theRest)]
  readsPrec _ ('M':'u':'l':'t':'i':'L':'a':'b':'e':'l':'-':'L':'o':'g':'L':'o':'s':'s':theRest) = [(MultiLabelLogLoss, theRest)]
  readsPrec _ ('M':'u':'l':'t':'i':'L':'a':'b':'e':'l':'-':'L':'i':'k':'e':'l':'i':'h':'o':'o':'d':theRest) = [(MultiLabelLikelihood, theRest)]



data MetricOrdering = TheLowerTheBetter | TheHigherTheBetter

-- | Returns what is preferred for a given metric: high values or low values.
getMetricOrdering :: Metric -> MetricOrdering
getMetricOrdering RMSE     = TheLowerTheBetter
getMetricOrdering MSE      = TheLowerTheBetter
getMetricOrdering Pearson  = TheHigherTheBetter
getMetricOrdering Spearman = TheHigherTheBetter
getMetricOrdering BLEU     = TheHigherTheBetter
getMetricOrdering GLEU     = TheHigherTheBetter
getMetricOrdering WER      = TheLowerTheBetter
getMetricOrdering Accuracy = TheHigherTheBetter
getMetricOrdering ClippEU  = TheHigherTheBetter
getMetricOrdering (FMeasure _) = TheHigherTheBetter
getMetricOrdering (MacroFMeasure _) = TheHigherTheBetter
getMetricOrdering (SoftFMeasure _) = TheHigherTheBetter
getMetricOrdering NMI = TheHigherTheBetter
getMetricOrdering (LogLossHashed _) = TheLowerTheBetter
getMetricOrdering (LikelihoodHashed _) = TheHigherTheBetter
getMetricOrdering CharMatch = TheHigherTheBetter
getMetricOrdering MAP = TheHigherTheBetter
getMetricOrdering LogLoss = TheLowerTheBetter
getMetricOrdering Likelihood = TheHigherTheBetter
getMetricOrdering BIOF1 = TheHigherTheBetter
getMetricOrdering BIOF1Labels = TheHigherTheBetter
getMetricOrdering MAE = TheLowerTheBetter
getMetricOrdering (MultiLabelFMeasure _) = TheHigherTheBetter
getMetricOrdering MultiLabelLogLoss = TheLowerTheBetter
getMetricOrdering MultiLabelLikelihood = TheHigherTheBetter


isInputNeeded :: Metric -> Bool
isInputNeeded CharMatch = True
isInputNeeded _ = False

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
                            gesOutFile :: String,
                            gesExpectedFile :: String,
                            gesInputFile :: String,
                            gesMetrics :: [Metric],
                            gesPrecision :: Maybe Int,
                            gesTokenizer :: Maybe Tokenizer,
                            gesGonitoHost :: Maybe String,
                            gesToken :: Maybe String }

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

data ResultOrdering = KeepTheOriginalOrder | FirstTheWorst | FirstTheBest

data GEvalOptions = GEvalOptions
                    { geoSpecialCommand :: Maybe GEvalSpecialCommand,
                      geoResultOrdering :: ResultOrdering,
                      geoSpec :: GEvalSpecification }

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

somethingWrongWithFilesMessage :: String -> FilePath -> String
somethingWrongWithFilesMessage msg filePath = Prelude.concat
                                [ msg, ": `", filePath, "`" ]

defaultGEvalSpecification = GEvalSpecification {
  gesOutDirectory = defaultOutDirectory,
  gesExpectedDirectory = Nothing,
  gesTestName = defaultTestName,
  gesOutFile = defaultOutFile,
  gesExpectedFile = defaultExpectedFile,
  gesInputFile = defaultInputFile,
  gesMetrics = [defaultMetric],
  gesPrecision = Nothing,
  gesTokenizer = Nothing,
  gesGonitoHost = Nothing,
  gesToken = Nothing }

isEmptyFile :: FilePath -> IO (Bool)
isEmptyFile path = do
    stat <- getFileStatus path
    return ((fileSize stat) == 0)


data LineSource m = LineSource (ConduitT () Text m ()) (Text -> Text) SourceSpec Word32

geval :: GEvalSpecification -> IO [(SourceSpec, [MetricValue])]
geval gevalSpec = do
  (inputSource, expectedSource, outSources) <- checkAndGetFiles False gevalSpec
  results <- Prelude.mapM (gevalOnSingleOut gevalSpec inputSource expectedSource) outSources
  return $ sortBy (\a b ->  (show $ fst a) `naturalComp` (show $ fst b)) results

gevalOnSingleOut :: GEvalSpecification -> SourceSpec -> SourceSpec -> SourceSpec -> IO (SourceSpec, [MetricValue])
gevalOnSingleOut gevalSpec inputSource expectedSource outSource = do
  vals <- Prelude.mapM (\metric -> gevalCore metric preprocess inputSource expectedSource outSource) metrics
  return (outSource, vals)
  where metrics = gesMetrics gevalSpec
        preprocess = gesPreprocess gevalSpec

checkAndGetFilesSingleOut :: Bool -> GEvalSpecification -> IO (SourceSpec, SourceSpec, SourceSpec)
checkAndGetFilesSingleOut forceInput gevalSpec = do
  res <- checkAndGetFiles forceInput gevalSpec
  case res of
    (inputSource, expectedSource, [outSource]) -> return (inputSource, expectedSource, outSource)
    _ -> throwM $ UnexpectedMultipleOutputs

checkAndGetFiles :: Bool -> GEvalSpecification -> IO (SourceSpec, SourceSpec, [SourceSpec])
checkAndGetFiles forceInput gevalSpec = do
  ess <- getSmartSourceSpec expectedTestDirectory "expected.tsv" expectedFile
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
           oss <- getSmartSourceSpec outTestDirectory "out.tsv" outFile
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

checkMultipleOuts :: GEvalSpecification -> IO (Maybe [FilePath])
checkMultipleOuts gevalSpec = checkMultipleOutsCore outDirectory testName outFile
  where outFile = gesOutFile gevalSpec
        outDirectory = gesOutDirectory gevalSpec
        testName = gesTestName gevalSpec

checkMultipleOutsCore :: FilePath -> FilePath -> FilePath -> IO (Maybe [FilePath])
checkMultipleOutsCore outDirectory testName outFile = do
  -- if the out.tsv is there, just use it
  outFilePath <- lookForCompressedFiles (outTestDirectory </> outFile)
  isSimpleOutThere <- D.doesFileExist outFilePath

  let patterns = Prelude.map (\ext -> compile ("out-*.tsv" ++ ext)) ["", ".gz", ".bz2", ".xz"]
  multipleOuts <- Prelude.concat <$> globDir patterns outTestDirectory

  if outFile == "out.tsv" && not isSimpleOutThere && multipleOuts /= []
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
       iss <- getSmartSourceSpec directory "in.tsv" inputFilePath
       case iss of
         Left NoSpecGiven -> throwM $ NoInputFile inputFilePath
         Left (NoFile fp) -> throwM $ NoInputFile fp
         Left (NoDirectory _) -> throwM $ NoInputFile inputFilePath
         Right sourceSpec -> return sourceSpec
   | otherwise = return NoSource

fileAsLineSource :: SourceSpec -> (Text -> Text) -> LineSource (ResourceT IO)
fileAsLineSource spec preprocess =
  LineSource ((smartSource spec) .| autoDecompress .| CT.decodeUtf8Lenient .| CT.lines) preprocess spec 1

gevalCoreOnSingleLines :: Metric -> (Text -> Text) -> LineInFile -> LineInFile -> LineInFile -> IO (MetricValue)
gevalCoreOnSingleLines metric preprocess inpLine expLine outLine =
  gevalCoreOnSources metric (singleLineAsLineSource inpLine preprocess)
                            (singleLineAsLineSource expLine preprocess)
                            (singleLineAsLineSource outLine preprocess)

singleLineAsLineSource :: LineInFile -> (Text -> Text) -> LineSource (ResourceT IO)
singleLineAsLineSource (LineInFile sourceSpec lineNo line) preprocess =
  LineSource (CL.sourceList [line]) preprocess sourceSpec lineNo

-- | Runs evaluation for a given metric using the sources specified
-- for input, expected output and output. Returns the metric value.
-- Throws @GEvalException@ if something was wrong in the data (e.g.
-- inconsistent number of lines in the sources).
gevalCore :: Metric           -- ^ evaluation metric
          -> (Text -> Text)    -- ^ preprocessing function (e.g. tokenization)
          -> SourceSpec       -- ^ source specification for the input values
          -> SourceSpec       -- ^ source specification for the expected output
          -> SourceSpec       -- ^ source specification for the output
          -> IO (MetricValue) -- ^ metric value for the output against the expected output
gevalCore metric preprocess inputSource expectedSource outSource = do
  whenM (isEmptyFileSource outSource) $ throwM $ EmptyOutput
  gevalCoreOnSources metric
                     (fileAsLineSource inputSource preprocess)
                     (fileAsLineSource expectedSource preprocess)
                     (fileAsLineSource outSource preprocess)

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
                     -> m (MetricValue)           -- ^ metric values for the output against the expected output
gevalCoreOnSources RMSE inputLineSource expectedLineSource outLineSource = do
  mse <- gevalCoreOnSources MSE inputLineSource expectedLineSource outLineSource
  return $ mse ** 0.5

gevalCoreOnSources Likelihood inputLineSource expectedLineSource outLineSource = do
  logLoss <- gevalCoreOnSources LogLoss inputLineSource expectedLineSource outLineSource
  return $ logLossToLikehood logLoss

gevalCoreOnSources (LikelihoodHashed b) inputLineSource expectedLineSource outLineSource = do
  logLoss <- gevalCoreOnSources (LogLossHashed b) inputLineSource expectedLineSource outLineSource
  return $ logLossToLikehood logLoss

gevalCoreOnSources MultiLabelLikelihood inputLineSource expectedLineSource outLineSource = do
  logLoss <- gevalCoreOnSources MultiLabelLogLoss inputLineSource expectedLineSource outLineSource
  return $ logLossToLikehood logLoss

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
           -> m (MetricValue)           -- ^ metric values for the output against the expected output
gevalCore' MSE _ = gevalCoreWithoutInput outParser outParser itemSquaredError averageC id
  where outParser = getValue . TR.double

gevalCore' MAE _ = gevalCoreWithoutInput outParser outParser itemAbsoluteError averageC id
  where outParser = getValue . TR.double

gevalCore' Pearson _ = gevalCoreByCorrelationMeasure pearson
gevalCore' Spearman _ = gevalCoreByCorrelationMeasure spearman

gevalCore' LogLoss _ = gevalCoreWithoutInput outParser outParser itemLogLossError averageC id
  where outParser = getValue . TR.double

gevalCore' BLEU _ = gevalCoreWithoutInput (Right . Prelude.map Prelude.words . DLS.splitOn "\t" . unpack) (Right . Prelude.words . unpack) bleuCombine bleuAgg bleuFinal
  where bleuFinal (p1, p2, p3, p4, rl, l1, l2, l3, l4) = ((p1 /. l1) * (p2 /. l2) * (p3 /. l3) * (p4 /. l4)) ** 0.25 * (brevityPenalty l1 rl)
        bleuCombine (refs, sen) = bleuStep refs sen
        bleuAgg = CC.foldl bleuFuse (0, 0, 0, 0, 0,  0, 0, 0, 0)
        bleuFuse (a1, a2, a3, a4, a5, a6, a7, a8, a9) (b1, b2, b3, b4, b5, b6, b7, b8, b9) = (a1+b1, a2+b2, a3+b3, a4+b4, a5+b5, a6+b6, a7+b7, a8+b8, a9+b9)
        brevityPenalty c r
          | c >= r = 1.0
          | c == 0 && r > 0 = 0.0
          | otherwise = exp (1.0 - (r /. c))

gevalCore' GLEU _ = gevalCoreWithoutInput (Right . Prelude.map Prelude.words . DLS.splitOn "\t" . unpack) (Right . Prelude.words . unpack) gleuCombine gleuAgg gleuFinal
  where gleuFinal (m, t) = m /. t
        gleuCombine (refs, sen) = gleuStep refs sen
        gleuAgg = CC.foldl gleuFuse (0, 0)
        gleuFuse (a1, a2) (b1, b2) = (a1+b1, a2+b2)

gevalCore' WER _ = gevalCoreWithoutInput (Right . Prelude.words . unpack) (Right . Prelude.words . unpack) (uncurry werStep) averageC id

gevalCore' Accuracy _ = gevalCoreWithoutInput (Right . strip) (Right . strip) hitOrMiss averageC id
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

gevalCore' (FMeasure beta) _ = gevalCoreWithoutInput outParser outParser getCount countAgg (fMeasureOnCounts beta)
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

gevalCore' (MacroFMeasure beta) _ = gevalCoreWithoutInput (Right . Just . strip) (Right . predicted . strip) getClassesInvolved gatherClassC macroAverageOnCounts
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
                                                         parseAnnotations
                                                         getSoftCounts
                                                         countAgg
                                                         (fMeasureOnCounts beta)
                      where getSoftCounts (expected, got) = (weightedMaxMatch matchScore expected got,
                                                             Prelude.length expected,
                                                             Prelude.length got)

gevalCore' ClippEU _ = gevalCoreWithoutInput parseClippingSpecs parseClippings matchStep clippeuAgg finalStep
  where
    parseClippings = controlledParse lineClippingsParser
    parseClippingSpecs = controlledParse lineClippingSpecsParser
    matchStep (clippingSpecs, clippings) = (maxMatch matchClippingToSpec clippingSpecs clippings,
                                            Prelude.length clippingSpecs,
                                            Prelude.length clippings)
    clippeuAgg = CC.foldl countFolder (0, 0, 0)
    finalStep counts = f2MeasureOnCounts counts

gevalCore' NMI _ = gevalCoreWithoutInput (Right . id) (Right . id) id (CC.foldl updateConfusionMatrix M.empty) normalizedMutualInformationFromConfusionMatrix

gevalCore' MAP _ = gevalCoreWithoutInput (Right . DLS.splitOn "\t" . unpack)
                                         (Right . DLS.splitOn "\t" . unpack)
                                         (\(e,g) -> calculateMAPForOneResult e g)
                                         averageC
                                         id

gevalCore' (LogLossHashed nbOfBits) _ = helper nbOfBits
  -- for LogLossHashed we "salt" each hash with the line number
  where helper nbOfBits expectedLineSource outLineSource =
          gevalCore''' (ParserSpecWithoutInput (Right . id) tentativeParser) (\(lineNo, (t,d)) -> calculateLogLoss nbOfBits lineNo t (parseDistributionWrapper nbOfBits lineNo d)) averageC negate (WithoutInput expectedLineSource outLineSource)
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
     gevalCoreGeneralized (ParserSpecWithInput (Right . unpack) (Right . unpack) (Right . unpack)) step countAgg (fMeasureOnCounts charMatchBeta) (WithInput inputLineSource expectedLineSource outputLineSource)
   step (ParsedRecordWithInput inp exp out) = getCharMatchCount inp exp out

gevalCore' BIOF1 _ = gevalCoreWithoutInput parseBioSequenceIntoEntities parseBioSequenceIntoEntities (uncurry gatherCountsForBIO) countAgg f1MeasureOnCounts

gevalCore' BIOF1Labels _ = gevalCoreWithoutInput parseBioSequenceIntoEntitiesWithoutNormalization parseBioSequenceIntoEntitiesWithoutNormalization (uncurry gatherCountsForBIO) countAgg f1MeasureOnCounts
   where parseBioSequenceIntoEntitiesWithoutNormalization s = do
           entities <- parseBioSequenceIntoEntities s
           return $ Prelude.map eraseNormalisation entities

gevalCore' (MultiLabelFMeasure beta) _ = gevalCoreWithoutInput intoWords
                                                               getWords
                                                               (getCounts (==))
                                                               countAgg
                                                               (fMeasureOnCounts beta)
    where
      getWords = Right . (Prelude.map unpack) . selectByStandardThreshold . parseIntoProbList
      intoWords = Right . (Prelude.map unpack) . Data.Text.words

gevalCore' MultiLabelLogLoss _ = gevalCoreWithoutInput intoWords
                                                       (Right . parseIntoProbList)
                                                       (uncurry countLogLossOnProbList)
                                                       averageC
                                                       id
    where
      intoWords = Right . Data.Text.words

countAgg :: (Num n, Monad m) => ConduitM (n, Int, Int) o m (n, Int, Int)
countAgg = CC.foldl countFolder (fromInteger 0, 0, 0)

gevalCoreByCorrelationMeasure :: (MonadUnliftIO m, MonadThrow m, MonadIO m) =>
                                (V.Vector (Double, Double) -> Double) -> -- ^ correlation function
                                LineSource (ResourceT m) ->  -- ^ source to read the expected output
                                LineSource (ResourceT m) ->  -- ^ source to read the output
                                m (MetricValue)             -- ^ metric values for the output against the expected output
gevalCoreByCorrelationMeasure correlationFunction =
  gevalCoreWithoutInput outParser outParser id correlationC finalStep
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
gevalCoreWithoutInput :: (MonadUnliftIO m, MonadThrow m, MonadIO m) =>
                      (Text -> Either String a) ->  -- ^ parser for values in the expected output
                      (Text -> Either String b) ->  -- ^ parser for values in the actual output
                      ((a, b) -> c) ->              -- ^ function which combines parsed values into a single value
                                                  -- (will be launched for each item, e.g. an error/cost function
                                                  -- could be calculated here)
                      (ConduitT c Void (ResourceT m) d) ->  -- ^ a Conduit which aggregates all the combined values into
                                                  -- a "total" value
                      (d -> Double) ->              -- ^ function to transform the "total" value into the final score
                      LineSource (ResourceT m) ->  -- ^ source to read the expected output
                      LineSource (ResourceT m) ->  -- ^ source to read the output
                      m (MetricValue)             -- ^ metric values for the output against the expected output
gevalCoreWithoutInput expParser outParser itemStep aggregator finalStep expectedLineStream outLineStream =
  gevalCoreGeneralized (ParserSpecWithoutInput expParser outParser) (trans itemStep) aggregator finalStep (WithoutInput expectedLineStream outLineStream)
 where
   trans :: ((a, b) -> c) -> ParsedRecord (WithoutInput m a b) -> c
   trans step (ParsedRecordWithoutInput x y) = step (x, y)

gevalCore''' :: (MonadUnliftIO m, MonadThrow m, MonadIO m) => ParserSpec (WithoutInput m a b) -> ((Word32, (a, b)) -> c) -> (ConduitT c Void (ResourceT m) d) -> (d -> Double) -> WithoutInput m a b -> m (MetricValue)
gevalCore''' parserSpec itemStep aggregator finalStep context =
  gevalCoreGeneralized' parserSpec (trans itemStep) aggregator finalStep context
 where
   trans :: ((Word32, (a, b)) -> c) -> (Word32, ParsedRecord (WithoutInput m a b)) -> c
   trans step (n, ParsedRecordWithoutInput x y) = step (n, (x, y))

-- | General function to run the evaluation, no matter which particular metric
-- was used. It could be seen as the "engine" to run the evaluation.
-- If you are defining a new metric, you usually don't have to change anything
-- here.
gevalCoreGeneralized :: (EvaluationContext ctxt m, MonadUnliftIO m, MonadThrow m, MonadIO m) =>
                     ParserSpec ctxt ->           -- ^ parsers to parse data
                     (ParsedRecord ctxt -> c) ->   -- ^ function to go from the parsed value into
                                                 -- some "local" score calculated for each line (item)
                     (ConduitT c Void (ResourceT m) d) ->  -- ^ a Conduit to aggregate score into a "total"
                     (d -> Double) ->              -- ^ function to transform the "total" value into the final score
                     ctxt ->                      -- ^ "context", i.e. 2 or 3 sources needed to operate
                     m (MetricValue)
gevalCoreGeneralized parserSpec itemStep aggregator finalStep context =
  gevalCoreGeneralized' parserSpec (skipLineNumber itemStep) aggregator finalStep context

gevalCoreGeneralized' :: forall m ctxt c d . (EvaluationContext ctxt m, MonadUnliftIO m, MonadThrow m, MonadIO m) => ParserSpec ctxt -> ((Word32, ParsedRecord ctxt) -> c) -> (ConduitT c Void (ResourceT m) d) -> (d -> Double) -> ctxt -> m (MetricValue)
gevalCoreGeneralized' parserSpec itemStep aggregator finalStep context = do
   v <- runResourceT $ runConduit $
     (((getZipSource $ (,)
       <$> ZipSource (CL.sourceList [(getFirstLineNo (Proxy :: Proxy m) context)..])
       <*> (ZipSource $ recordSource context parserSpec)) .| CL.map (checkStep (Proxy :: Proxy m) itemStep)) .| CL.catMaybes .| aggregator)
   return $ finalStep v

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
  data ParserSpec (WithoutInput m e o) = ParserSpecWithoutInput (Text -> Either String e) (Text -> Either String o)
  data WrappedParsedRecord (WithoutInput m e o) = WrappedParsedRecordWithoutInput (SourceItem e) (SourceItem o)
  data ParsedRecord (WithoutInput m e o) = ParsedRecordWithoutInput e o
  getFirstLineNo _ (WithoutInput _ (LineSource _ _ _ lineNo)) = lineNo
  getExpectedSource (WithoutInput (LineSource _ _ expectedSource _) _) = expectedSource
  getOutSource (WithoutInput _ (LineSource _ _ outSource _)) = outSource
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

getInputFilePath (WithInput (LineSource _ _ inputFilePath _) _ _) = inputFilePath

instance (MonadUnliftIO m, MonadIO m, MonadThrow m) => EvaluationContext (WithInput m i e o) m where
  data ParserSpec (WithInput m i e o) = ParserSpecWithInput (Text -> Either String i) (Text -> Either String e) (Text -> Either String o)
  data WrappedParsedRecord (WithInput m i e o) = WrappedParsedRecordWithInput (SourceItem i) (SourceItem e) (SourceItem o)
  data ParsedRecord (WithInput m i e o) = ParsedRecordWithInput i e o
  getFirstLineNo _ (WithInput _ _ (LineSource _ _ _ lineNo)) = lineNo
  getExpectedSource (WithInput _ (LineSource _ _ expectedSource _) _) = expectedSource
  getOutSource (WithInput _ _ (LineSource _ _ outSource _)) = outSource
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




averageC :: MonadResource m => ConduitT Double Void m Double
averageC = getZipSink
    $ (\total count -> total / fromIntegral count)
  <$> ZipSink CC.sum
  <*> ZipSink CC.length

items :: MonadResource m => LineSource m -> (Text -> Either String a) -> ConduitT () (SourceItem a) m ()
items (LineSource lineSource preprocess _ _) parser =
  (lineSource .| CL.map (toItem . parser . preprocess)) >> yield Done
  where toItem (Right x) = Got x
        toItem (Left m) = Wrong m

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
