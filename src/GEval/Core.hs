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
      checkAndGetFiles
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
import Control.Conditional (unlessM, whenM)
import qualified System.Directory as D
import System.Posix
import System.FilePath
import Data.Maybe
import qualified Data.List.Split as DLS

import Control.Monad.IO.Class
import Control.Monad ((<=<))

import Data.Attoparsec.Text (parseOnly)

import GEval.BLEU
import GEval.Common
import GEval.ClippEU
import GEval.PrecisionRecall
import GEval.ClusteringMetrics
import GEval.LogLossHashed
import GEval.CharMatch

import qualified Data.HashMap.Strict as M

import Data.Proxy

import Data.Word

type MetricValue = Double

defaultLogLossHashedSize :: Word32
defaultLogLossHashedSize = 10

data Metric = RMSE | MSE | BLEU | Accuracy | ClippEU | FMeasure Double | NMI | LogLossHashed Word32 | CharMatch
              | MAP
              deriving (Eq)

instance Show Metric where
  show RMSE = "RMSE"
  show MSE  = "MSE"
  show BLEU = "BLEU"
  show Accuracy = "Accuracy"
  show ClippEU = "ClippEU"
  show (FMeasure beta) = "F" ++ (show beta)
  show NMI = "NMI"
  show (LogLossHashed nbOfBits) = "LogLossHashed" ++ (if
                                                       nbOfBits == defaultLogLossHashedSize
                                                      then
                                                       ""
                                                      else
                                                       (show nbOfBits))
  show CharMatch = "CharMatch"
  show MAP = "MAP"

instance Read Metric where
  readsPrec _ ('R':'M':'S':'E':theRest) = [(RMSE, theRest)]
  readsPrec _ ('M':'S':'E':theRest) = [(MSE, theRest)]
  readsPrec _ ('B':'L':'E':'U':theRest) = [(BLEU, theRest)]
  readsPrec _ ('A':'c':'c':'u':'r':'a':'c':'y':theRest) = [(Accuracy, theRest)]
  readsPrec _ ('C':'l':'i':'p':'p':'E':'U':theRest) = [(ClippEU, theRest)]
  readsPrec _ ('N':'M':'I':theRest) = [(NMI, theRest)]
  readsPrec p ('F':theRest) = case readsPrec p theRest of
    [(beta, theRest)] -> [(FMeasure beta, theRest)]
    _ -> []
  readsPrec p ('L':'o':'g':'L':'o':'s':'s':'H':'a':'s':'h':'e':'d':theRest) = case readsPrec p theRest of
    [(nbOfBits, theRest)] -> [(LogLossHashed nbOfBits, theRest)]
    _ -> [(LogLossHashed defaultLogLossHashedSize, theRest)]
  readsPrec p ('C':'h':'a':'r':'M':'a':'t':'c':'h':theRest) = [(CharMatch, theRest)]
  readsPrec _ ('M':'A':'P':theRest) = [(MAP, theRest)]

data MetricOrdering = TheLowerTheBetter | TheHigherTheBetter

getMetricOrdering :: Metric -> MetricOrdering
getMetricOrdering RMSE     = TheLowerTheBetter
getMetricOrdering MSE      = TheLowerTheBetter
getMetricOrdering BLEU     = TheHigherTheBetter
getMetricOrdering Accuracy = TheHigherTheBetter
getMetricOrdering ClippEU  = TheHigherTheBetter
getMetricOrdering (FMeasure _) = TheHigherTheBetter
getMetricOrdering NMI = TheHigherTheBetter
getMetricOrdering (LogLossHashed _) = TheLowerTheBetter
getMetricOrdering CharMatch = TheHigherTheBetter
getMetricOrdering MAP = TheHigherTheBetter

defaultOutDirectory = "."
defaultTestName = "test-A"
defaultOutFile = "out.tsv"
defaultExpectedFile = "expected.tsv"
defaultInputFile = "in.tsv"

defaultMetric :: Metric
defaultMetric = RMSE

configFileName :: FilePath
configFileName = "config.txt"

data GEvalSpecification = GEvalSpecification
                          { gesOutDirectory :: FilePath,
                            gesExpectedDirectory :: Maybe FilePath,
                            gesTestName :: String,
                            gesOutFile :: String,
                            gesExpectedFile :: String,
                            gesInputFile :: String,
                            gesMetric :: Metric }

getExpectedDirectory :: GEvalSpecification -> FilePath
getExpectedDirectory spec = fromMaybe outDirectory $ gesExpectedDirectory spec
                            where outDirectory = gesOutDirectory spec

data GEvalSpecialCommand = Init | LineByLine

data GEvalOptions = GEvalOptions
                    { geoSpecialCommand :: Maybe GEvalSpecialCommand,
                      geoPrecision :: Maybe Int,
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
  gesMetric = defaultMetric }

isEmptyFile :: FilePath -> IO (Bool)
isEmptyFile path = do
    stat <- getFileStatus path
    return ((fileSize stat) == 0)


data LineSource m = LineSource (Source m Text) FilePath Word32

geval :: GEvalSpecification -> IO (MetricValue)
geval gevalSpec = do
  (inputFilePath, expectedFilePath, outFilePath) <- checkAndGetFiles gevalSpec
  gevalCore metric inputFilePath expectedFilePath outFilePath
   where metric = gesMetric gevalSpec

checkAndGetFiles :: GEvalSpecification -> IO (FilePath, FilePath, FilePath)
checkAndGetFiles gevalSpec = do
  unlessM (D.doesDirectoryExist outDirectory) $ throwM $ NoOutDirectory outDirectory
  unlessM (D.doesDirectoryExist expectedDirectory) $ throwM $ NoExpectedDirectory expectedDirectory
  unlessM (D.doesDirectoryExist outTestDirectory) $ throwM $ NoOutTestDirectory outTestDirectory
  unlessM (D.doesDirectoryExist expectedTestDirectory) $ throwM $ NoExpectedTestDirectory expectedTestDirectory
  checkInputFileIfNeeded metric inputFilePath
  return (inputFilePath, expectedFilePath, outFilePath)
   where expectedFilePath = expectedTestDirectory </> (gesExpectedFile gevalSpec)
         outFilePath = outTestDirectory </> (gesOutFile gevalSpec)
         inputFilePath = expectedTestDirectory </> (gesInputFile gevalSpec)
         expectedTestDirectory = expectedDirectory </> testName
         outTestDirectory = outDirectory </> testName
         expectedDirectory = getExpectedDirectory gevalSpec
         outDirectory = gesOutDirectory gevalSpec
         testName = gesTestName gevalSpec
         metric = gesMetric gevalSpec

checkInputFileIfNeeded :: Metric -> FilePath -> IO ()
checkInputFileIfNeeded CharMatch inputFilePath = do
  unlessM (D.doesFileExist inputFilePath) $ throwM $ NoInputFile inputFilePath
  return ()
checkInputFileIfNeeded _ _ = return ()

fileAsLineSource :: FilePath -> LineSource (ResourceT IO)
fileAsLineSource filePath =
  LineSource (CB.sourceFile filePath $= CT.decodeUtf8Lenient =$= CT.lines) filePath 1

gevalCoreOnSingleLines :: Metric -> LineInFile -> LineInFile -> LineInFile -> IO (MetricValue)
gevalCoreOnSingleLines metric inpLine expLine outLine =
  gevalCoreOnSources metric (singleLineAsLineSource inpLine)
                            (singleLineAsLineSource expLine)
                            (singleLineAsLineSource outLine)

singleLineAsLineSource :: LineInFile -> LineSource (ResourceT IO)
singleLineAsLineSource (LineInFile filePath lineNo line) =
  LineSource (CL.sourceList [line]) filePath lineNo

gevalCore :: Metric -> String -> String -> String -> IO (MetricValue)
gevalCore metric inputFilePath expectedFilePath outFilePath = do
  unlessM (D.doesFileExist expectedFilePath) $ throwM $ NoExpectedFile expectedFilePath
  unlessM (D.doesFileExist outFilePath) $ throwM $ NoOutFile outFilePath
  whenM (isEmptyFile outFilePath) $ throwM $ EmptyOutput
  gevalCoreOnSources metric
                     (fileAsLineSource inputFilePath)
                     (fileAsLineSource expectedFilePath)
                     (fileAsLineSource outFilePath)

gevalCoreOnSources :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => Metric
                     -> LineSource (ResourceT m)
                     -> LineSource (ResourceT m)
                     -> LineSource (ResourceT m)
                     -> m (MetricValue)
gevalCoreOnSources RMSE inputLineSource expectedLineSource outLineSource = do
  mse <- gevalCoreOnSources MSE inputLineSource expectedLineSource outLineSource
  return $ mse ** 0.5

gevalCoreOnSources metric inputLineSource expectedLineSource outLineSource = do
  gevalCore' metric inputLineSource expectedLineSource outLineSource

data LineInFile = LineInFile FilePath Word32 Text

gevalCore' :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) => Metric -> LineSource (ResourceT m) -> LineSource (ResourceT m) -> LineSource (ResourceT m) -> m (MetricValue)
gevalCore' MSE _ = gevalCoreWithoutInput outParser outParser itemError averageC id
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

gevalCore' Accuracy _ = gevalCoreWithoutInput (Right . strip) (Right . strip) hitOrMiss averageC id
                      where hitOrMiss (x,y) = if x == y then 1.0 else 0.0

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
        getCount (True, True)   = (1, 1, 1)
        getCount (True, False)  = (0, 1, 0)
        getCount (False, True)  = (0, 0, 1)
        getCount (False, False) = (0, 0, 0)
        countAgg = CC.foldl countFolder (0, 0, 0)

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
   countAgg = CC.foldl countFolder (0, 0, 0)

parseDistributionWrapper :: Word32 -> Word32 -> Text -> HashedDistribution
parseDistributionWrapper nbOfBits seed distroSpec = case parseDistribution nbOfBits seed distroSpec of
  Right distro -> distro
  Left s -> throw $ UnexpectedData 0 s -- shouldn't be here anyway

data SourceItem a = Got a | Wrong String | Done

skipLineNumber :: (x -> c) -> ((Word32, x) -> c)
skipLineNumber fun = fun . snd

gevalCoreWithoutInput :: (MonadBaseControl IO m, MonadThrow m, MonadIO m) => (Text -> Either String a) -> (Text -> Either String b) -> ((a, b) -> c) -> (Sink c (ResourceT m) d) -> (d -> Double) -> LineSource (ResourceT m) -> LineSource (ResourceT m) -> m (MetricValue)
gevalCoreWithoutInput expParser outParser itemStep aggregator finalStep expectedLineStream outLineStream =
  gevalCoreGeneralized (ParserSpecWithoutInput expParser outParser) (trans itemStep) aggregator finalStep (WithoutInput expectedLineStream outLineStream)
 where
   trans :: ((a, b) -> c) -> ParsedRecord (WithoutInput m a b) -> c
   trans step (ParsedRecordWithoutInput x y) = step (x, y)

gevalCore''' :: (MonadBaseControl IO m, MonadThrow m, MonadIO m) => ParserSpec (WithoutInput m a b) -> ((Word32, (a, b)) -> c) -> (Sink c (ResourceT m) d) -> (d -> Double) -> WithoutInput m a b -> m (MetricValue)
gevalCore''' parserSpec itemStep aggregator finalStep context =
  gevalCoreGeneralized' parserSpec (trans itemStep) aggregator finalStep context
 where
   trans :: ((Word32, (a, b)) -> c) -> (Word32, ParsedRecord (WithoutInput m a b)) -> c
   trans step (n, ParsedRecordWithoutInput x y) = step (n, (x, y))

gevalCoreGeneralized :: (EvaluationContext ctxt m, MonadBaseControl IO m, MonadThrow m, MonadIO m) => ParserSpec ctxt -> (ParsedRecord ctxt -> c) -> (Sink c (ResourceT m) d) -> (d -> Double) -> ctxt -> m (MetricValue)
gevalCoreGeneralized parserSpec itemStep aggregator finalStep context =
  gevalCoreGeneralized' parserSpec (skipLineNumber itemStep) aggregator finalStep context

gevalCoreGeneralized' :: forall m ctxt c d . (EvaluationContext ctxt m, MonadBaseControl IO m, MonadThrow m, MonadIO m) => ParserSpec ctxt -> ((Word32, ParsedRecord ctxt) -> c) -> (Sink c (ResourceT m) d) -> (d -> Double) -> ctxt -> m (MetricValue)
gevalCoreGeneralized' parserSpec itemStep aggregator finalStep context = do
   v <- runResourceT $
     (((getZipSource $ (,)
       <$> ZipSource (CL.sourceList [1..])
       <*> (ZipSource $ recordSource context parserSpec)) =$= CL.map (checkStep (Proxy :: Proxy m) itemStep)) $$ CL.catMaybes =$ aggregator)
   return $ finalStep v

class EvaluationContext ctxt m where
  data ParserSpec ctxt :: *
  data WrappedParsedRecord ctxt :: *
  data ParsedRecord ctxt :: *
  recordSource :: ctxt -> ParserSpec ctxt -> Source (ResourceT m) (WrappedParsedRecord ctxt)
  getExpectedFilePath :: ctxt -> String
  getOutFilePath :: ctxt -> String
  checkStep :: Proxy m -> ((Word32, ParsedRecord ctxt) -> c) -> (Word32, WrappedParsedRecord ctxt) -> Maybe c
  checkStepM :: ((Word32, ParsedRecord ctxt) -> (ResourceT m) c) -> (Word32, WrappedParsedRecord ctxt) -> (ResourceT m) (Maybe c)

data WithoutInput m e o = WithoutInput (LineSource (ResourceT m)) (LineSource (ResourceT m))

instance (MonadBaseControl IO m, MonadIO m, MonadThrow m) => EvaluationContext (WithoutInput m e o) m where
  data ParserSpec (WithoutInput m e o) = ParserSpecWithoutInput (Text -> Either String e) (Text -> Either String o)
  data WrappedParsedRecord (WithoutInput m e o) = WrappedParsedRecordWithoutInput (SourceItem e) (SourceItem o)
  data ParsedRecord (WithoutInput m e o) = ParsedRecordWithoutInput e o
  getExpectedFilePath (WithoutInput (LineSource _ expectedFilePath _) _) = expectedFilePath
  getOutFilePath (WithoutInput _ (LineSource _ outFilePath _)) = outFilePath
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

getInputFilePath (WithInput (LineSource _ inputFilePath _) _ _) = inputFilePath

instance (MonadBaseControl IO m, MonadIO m, MonadThrow m) => EvaluationContext (WithInput m i e o) m where
  data ParserSpec (WithInput m i e o) = ParserSpecWithInput (Text -> Either String i) (Text -> Either String e) (Text -> Either String o)
  data WrappedParsedRecord (WithInput m i e o) = WrappedParsedRecordWithInput (SourceItem i) (SourceItem e) (SourceItem o)
  data ParsedRecord (WithInput m i e o) = ParsedRecordWithInput i e o
  getExpectedFilePath (WithInput _ (LineSource _ expectedFilePath _) _) = expectedFilePath
  getOutFilePath (WithInput _ _ (LineSource _ outFilePath _)) = outFilePath
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




averageC :: MonadResource m => Sink Double m Double
averageC = getZipSink
    $ (\total count -> total / fromIntegral count)
  <$> ZipSink CC.sum
  <*> ZipSink CC.length

items :: MonadResource m => LineSource m -> (Text -> Either String a) -> Source m (SourceItem a)
items (LineSource lineSource _ _) parser =
  (lineSource =$= CL.map (toItem . parser)) >> yield Done
  where toItem (Right x) = Got x
        toItem (Left m) = Wrong m

itemError :: (Double, Double) -> Double
itemError (exp, out) = (exp-out)**2

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
