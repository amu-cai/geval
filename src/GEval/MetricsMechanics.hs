{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module GEval.MetricsMechanics
  where

import Data.Singletons.TH

import Text.Read (readMaybe)

import GEval.Metric
import GEval.Common
import GEval.BLEU (bleuStep, gleuStep)
import GEval.WER (werStep)
import GEval.Clippings (totalArea, coveredBy, clippEUMatchStep,
                        ObtainedLabeledClipping(..), lineObtainedLabeledClippingsParser)
import GEval.BIO (gatherCountsForBIO, gatherSeparatedCountsForBIO)

import GEval.Probability
import GEval.PrecisionRecall (weightedMaxMatch, calculateMAPForOneResult, getProbabilisticCounts)

import Control.Exception

import Data.Text hiding (map, maximum, zip, filter)
import Data.Text.Read as TR
import qualified Data.List.Split as DLS
import Data.Attoparsec.Text (parseOnly)
import Data.Maybe (catMaybes)

import Control.Monad ((<=<))

import GEval.Annotation (Annotation, ObtainedAnnotation,
                         parseAnnotations, parseObtainedAnnotations,
                         parseSegmentAnnotations, segmentAccuracy)
import GEval.Clippings (Clipping, ClippingSpec, LabeledClipping, lineClippingsParser, lineClippingSpecsParser, lineLabeledClippingsParser)
import GEval.BIO (TaggedEntity, parseBioSequenceIntoEntities, parseBioSequenceIntoEntitiesWithoutNormalization)
import GEval.LogLossHashed (parseWordSpecs, wordSpecToPair)
import GEval.ProbList (ProbList(..), WordWithProb(..),
                       parseIntoProbList, countLogLossOnProbList, selectByStandardThreshold)
import GEval.MatchingSpecification
import GEval.Haversine

import qualified Data.HashMap.Strict as M

-- | Helper type so that singleton can be used.
-- | (The problem is that some metrics are parametrized by Double
-- | Word32 and this is not handled by the singleton libary.)
singletons [d|data AMetric = ARMSE | AMSE | APearson | ASpearman | ABLEU | AGLEU | AWER | ACER | AAccuracy MatchingSpecification | AClippEU
                             | AFMeasure | AMacroFMeasure | ANMI
                             | ALogLossHashed | ACharMatch | AMAP | ALogLoss | ALikelihood
                             | ABIOF1 | ABIOWeightedF1 | ABIOF1Labels | ATokenAccuracy | ASegmentAccuracy | ALikelihoodHashed | APerplexityHashed
                             | AMAE | ASMAPE | AMultiLabelFMeasure MatchingSpecification
                             | AMultiLabelLogLoss | AMultiLabelLikelihood
                             | ASoftFMeasure
                             | AProbabilisticMultiLabelFMeasure
                             | AProbabilisticSoftFMeasure
                             | AProbabilisticSoft2DFMeasure
                             | ASoft2DFMeasure
                             | AFLCFMeasure | AHaversine | AImprovement
                             deriving (Eq)
             |]

-- | Convert a metric to a helper type without parameters
toHelper :: Metric -> AMetric
toHelper RMSE = ARMSE
toHelper MSE = AMSE
toHelper Pearson = APearson
toHelper Spearman = ASpearman
toHelper BLEU = ABLEU
toHelper GLEU = AGLEU
toHelper WER = AWER
toHelper CER = ACER
toHelper (Accuracy matchingSpec) = AAccuracy matchingSpec
toHelper ClippEU = AClippEU
toHelper (FMeasure _) = AFMeasure
toHelper (MacroFMeasure _) = AMacroFMeasure
toHelper NMI = ANMI
toHelper (LogLossHashed _) = ALogLossHashed
toHelper CharMatch = ACharMatch
toHelper MAP = AMAP
toHelper LogLoss = ALogLoss
toHelper Likelihood = ALikelihood
toHelper BIOF1 = ABIOF1
toHelper BIOWeightedF1 = ABIOWeightedF1
toHelper BIOF1Labels = ABIOF1Labels
toHelper TokenAccuracy = ATokenAccuracy
toHelper SegmentAccuracy = ASegmentAccuracy
toHelper (LikelihoodHashed _) = ALikelihoodHashed
toHelper (PerplexityHashed _) = APerplexityHashed
toHelper MAE = AMAE
toHelper SMAPE = ASMAPE
toHelper (MultiLabelFMeasure _ matchingSpec) = AMultiLabelFMeasure matchingSpec
toHelper MultiLabelLogLoss = AMultiLabelLogLoss
toHelper MultiLabelLikelihood = AMultiLabelLikelihood
toHelper (SoftFMeasure _) = ASoftFMeasure
toHelper (FLCFMeasure _) = AFLCFMeasure
toHelper (ProbabilisticMultiLabelFMeasure _) = AProbabilisticMultiLabelFMeasure
toHelper (ProbabilisticSoftFMeasure _) = AProbabilisticSoftFMeasure
toHelper (ProbabilisticSoft2DFMeasure _) = AProbabilisticSoft2DFMeasure
toHelper (Soft2DFMeasure _) = ASoft2DFMeasure
toHelper Haversine = AHaversine
toHelper (Improvement _) = AImprovement

type family ParsedInputType (t :: AMetric) :: * where
  ParsedInputType ACharMatch = Text
  ParsedInputType _ = ()

type family ParsedExpectedType (t :: AMetric) :: * where
  ParsedExpectedType ARMSE     = Double
  ParsedExpectedType AMSE      = Double
  ParsedExpectedType APearson  = Double
  ParsedExpectedType ASpearman = Double
  ParsedExpectedType ABLEU     = [[String]]
  ParsedExpectedType AGLEU     = [[String]]
  ParsedExpectedType AWER      = [String]
  ParsedExpectedType ACER      = String
  ParsedExpectedType (AAccuracy _) = Text
  ParsedExpectedType AClippEU  = [ClippingSpec]
  ParsedExpectedType AFMeasure = Bool
  ParsedExpectedType AMacroFMeasure = Maybe Text
  ParsedExpectedType ASoftFMeasure = [Annotation]
  ParsedExpectedType AFLCFMeasure = [Annotation]
  ParsedExpectedType AProbabilisticMultiLabelFMeasure = [Text]
  ParsedExpectedType AProbabilisticSoftFMeasure = [Annotation]
  ParsedExpectedType AProbabilisticSoft2DFMeasure = [LabeledClipping]
  ParsedExpectedType ASoft2DFMeasure = [LabeledClipping]
  ParsedExpectedType ANMI = Text
  ParsedExpectedType ALogLossHashed = Text
  ParsedExpectedType ALikelihoodHashed = Text
  ParsedExpectedType APerplexityHashed = Text
  ParsedExpectedType ACharMatch = Text
  ParsedExpectedType AMAP = [String]
  ParsedExpectedType ALogLoss = Double
  ParsedExpectedType ALikelihood = Double
  ParsedExpectedType ABIOF1 = [TaggedEntity]
  ParsedExpectedType ABIOWeightedF1 = [TaggedEntity]
  ParsedExpectedType ABIOF1Labels = [TaggedEntity]
  ParsedExpectedType ATokenAccuracy = [Text]
  ParsedExpectedType ASegmentAccuracy = [Annotation]
  ParsedExpectedType AMAE = Double
  ParsedExpectedType ASMAPE = Double
  ParsedExpectedType (AMultiLabelFMeasure _) = [Text]
  ParsedExpectedType AMultiLabelLogLoss = [Text]
  ParsedExpectedType AMultiLabelLikelihood = [Text]
  ParsedExpectedType AHaversine = (Double, Double)
  ParsedExpectedType AImprovement = Double

expectedParser :: SAMetric t -> Text -> Either String (ParsedExpectedType t)
expectedParser SARMSE = doubleParser
expectedParser SAMSE = doubleParser
expectedParser SAPearson = doubleParser
expectedParser SASpearman = doubleParser
expectedParser SABLEU = alternativeSentencesParser
expectedParser SAGLEU = alternativeSentencesParser
expectedParser SAWER = intoStringWords
expectedParser SACER = Right . unpack
expectedParser (SAAccuracy _) = onlyStrip
expectedParser SAClippEU = controlledParse lineClippingSpecsParser
expectedParser SAFMeasure = zeroOneParser
expectedParser SAMacroFMeasure = justStrip
expectedParser SASoftFMeasure = parseAnnotations
expectedParser SAFLCFMeasure = parseAnnotations
expectedParser SAProbabilisticMultiLabelFMeasure = intoWords
expectedParser SAProbabilisticSoftFMeasure = parseAnnotations
expectedParser SAProbabilisticSoft2DFMeasure = controlledParse lineLabeledClippingsParser
expectedParser SASoft2DFMeasure = controlledParse lineLabeledClippingsParser
expectedParser SANMI = Right . id
expectedParser SALogLossHashed = onlyStrip
expectedParser SALikelihoodHashed = onlyStrip
expectedParser SAPerplexityHashed = onlyStrip
expectedParser SACharMatch = Right
expectedParser SAMAP = splitByTabs
expectedParser SALogLoss = doubleParser
expectedParser SALikelihood = doubleParser
expectedParser SABIOF1 = parseBioSequenceIntoEntities
expectedParser SABIOWeightedF1 = parseBioSequenceIntoEntities
expectedParser SABIOF1Labels = parseBioSequenceIntoEntitiesWithoutNormalization
expectedParser SATokenAccuracy = intoWords
expectedParser SASegmentAccuracy = parseSegmentAnnotations
expectedParser SAMAE = doubleParser
expectedParser SASMAPE = doubleParser
expectedParser (SAMultiLabelFMeasure _) = intoWords
expectedParser SAMultiLabelLogLoss = intoWords
expectedParser SAMultiLabelLikelihood = intoWords
expectedParser SAHaversine =  parseSpherePoints
expectedParser SAImprovement = doubleParser

type family ParsedOutputType (t :: AMetric) :: * where
  ParsedOutputType ABLEU = [String]
  ParsedOutputType AGLEU = [String]
  ParsedOutputType AClippEU = [Clipping]
  ParsedOutputType AMacroFMeasure = Maybe Text
  ParsedOutputType ASoftFMeasure = [ObtainedAnnotation]
  ParsedOutputType AFLCFMeasure = [ObtainedAnnotation]
  ParsedOutputType AProbabilisticMultiLabelFMeasure = [WordWithProb]
  ParsedOutputType AProbabilisticSoftFMeasure = [ObtainedAnnotation]
  ParsedOutputType AProbabilisticSoft2DFMeasure = [ObtainedLabeledClipping]
  ParsedOutputType AMultiLabelLikelihood = ProbList
  ParsedOutputType AMultiLabelLogLoss = ProbList
  ParsedOutputType AHaversine = (Double, Double)
  ParsedOutputType AImprovement = Double
  ParsedOutputType ASoft2DFMeasure = [ObtainedLabeledClipping]
  ParsedOutputType t = ParsedExpectedType t

outputParser :: SAMetric t -> Text -> Either String (ParsedOutputType t)
outputParser SARMSE = expectedParser SARMSE
outputParser SAMSE = expectedParser SARMSE
outputParser SAPearson = expectedParser SAPearson
outputParser SASpearman = expectedParser SASpearman
outputParser SABLEU = Right . Prelude.words . unpack
outputParser SAGLEU = Right . Prelude.words . unpack
outputParser SAWER = expectedParser SAWER
outputParser SACER = expectedParser SACER
outputParser p@(SAAccuracy _) = expectedParser p
outputParser SAClippEU = controlledParse lineClippingsParser
outputParser SAFMeasure = probToZeroOneParser
outputParser SAMacroFMeasure = Right . predictedParser . strip
outputParser SASoftFMeasure = parseObtainedAnnotations
outputParser SAFLCFMeasure = parseObtainedAnnotations
outputParser SAProbabilisticMultiLabelFMeasure = (Right . (\(ProbList es) -> es) . parseIntoProbList)
outputParser SAProbabilisticSoftFMeasure = parseObtainedAnnotations
outputParser SAProbabilisticSoft2DFMeasure = controlledParse lineObtainedLabeledClippingsParser
outputParser SASoft2DFMeasure = controlledParse lineObtainedLabeledClippingsParser
outputParser SANMI = expectedParser SANMI
outputParser SALogLossHashed = onlyStrip
outputParser SALikelihoodHashed = onlyStrip
outputParser SAPerplexityHashed = onlyStrip
outputParser SACharMatch = Right
outputParser SAMAP = splitByTabs
outputParser SALogLoss = doubleParser
outputParser SALikelihood = doubleParser
outputParser SABIOF1 = parseBioSequenceIntoEntities
outputParser SABIOWeightedF1 = parseBioSequenceIntoEntities
outputParser SABIOF1Labels = parseBioSequenceIntoEntitiesWithoutNormalization
outputParser SATokenAccuracy = intoWords
outputParser SASegmentAccuracy = parseSegmentAnnotations
outputParser SAMAE = doubleParser
outputParser SASMAPE = doubleParser
outputParser (SAMultiLabelFMeasure _) = Right . selectByStandardThreshold . parseIntoProbList
outputParser SAMultiLabelLogLoss = Right . parseIntoProbList
outputParser SAMultiLabelLikelihood = Right . parseIntoProbList
outputParser SAHaversine = parseSpherePoints
outputParser SAImprovement = doubleParser

type family ItemIntermediateRepresentationType (t :: AMetric) :: * where
  ItemIntermediateRepresentationType ABLEU = (Int, Int, Int, Int, Int, Int, Int, Int, Int)
  ItemIntermediateRepresentationType AGLEU = (Int, Int)
  ItemIntermediateRepresentationType AFMeasure = (Int, Int, Int)
  ItemIntermediateRepresentationType AMacroFMeasure = (Maybe Text, Maybe Text, Maybe Text)
  ItemIntermediateRepresentationType ASoftFMeasure = (Double, Int, Int)
  ItemIntermediateRepresentationType AFLCFMeasure = (Double, Double, Int, Int)
  ItemIntermediateRepresentationType ASoft2DFMeasure = (Integer, Integer, Integer)
  ItemIntermediateRepresentationType AClippEU = (Int, Int, Int)
  ItemIntermediateRepresentationType ANMI = (Text, Text)
  ItemIntermediateRepresentationType ABIOF1 = (Int, Int, Int)
  ItemIntermediateRepresentationType ABIOWeightedF1 = M.HashMap Text (Int, Int, Int)
  ItemIntermediateRepresentationType ABIOF1Labels = (Int, Int, Int)
  ItemIntermediateRepresentationType ATokenAccuracy = (Int, Int)
  ItemIntermediateRepresentationType AProbabilisticMultiLabelFMeasure = ([Double], [Double], Double, Int)
  ItemIntermediateRepresentationType AProbabilisticSoftFMeasure = ([Double], [Double], Double, Int)
  ItemIntermediateRepresentationType AProbabilisticSoft2DFMeasure = ([Double], [Double], Double, Int)
  ItemIntermediateRepresentationType APearson = (Double, Double)
  ItemIntermediateRepresentationType ASpearman = (Double, Double)
  -- FIXME
  -- It would be better to distinguish ExactMatch here (for which we could return (Int, Int, Int)
  -- ant other possibilities, but it resulted in too much down-the-rabbit hole with types.
  ItemIntermediateRepresentationType (AMultiLabelFMeasure _) = (Double, Int, Int)
  ItemIntermediateRepresentationType ALogLossHashed = (Text, Text)
  ItemIntermediateRepresentationType ALikelihoodHashed = (Text, Text)
  ItemIntermediateRepresentationType APerplexityHashed = (Text, Text)
  ItemIntermediateRepresentationType ACharMatch = (Text, Text)
  ItemIntermediateRepresentationType AWER = (Int, Int)
  ItemIntermediateRepresentationType ACER = (Int, Int)
  ItemIntermediateRepresentationType AHaversine = Double
  ItemIntermediateRepresentationType AImprovement = (Double, Double)
  ItemIntermediateRepresentationType t = Double

findBest :: (Text -> Text -> Double) -> (Text -> Text -> Double)
findBest fun expected got = maximum $ map (fun got) expectedVals
  where expectedVals = case splitOn "\t" expected of
                         [] -> [""]
                         l -> l


itemStep :: SAMetric t -> (ParsedExpectedType t, ParsedOutputType t) -> ItemIntermediateRepresentationType t
itemStep SARMSE = itemSquaredError
itemStep SAMSE = itemSquaredError
itemStep SAPearson = id
itemStep SASpearman = id
itemStep SABLEU = uncurry bleuStep
itemStep SAGLEU = uncurry gleuStep
itemStep SAWER = uncurry werStep
-- strings are character lists, so we could re-use werStep
itemStep SACER = uncurry werStep
itemStep (SAAccuracy SExactMatch) = hitOrMiss
itemStep (SAAccuracy smatchSpec) = uncurry (findBest $ getMatchingFunctionForText $ fromSing smatchSpec)
itemStep SAClippEU = clippEUMatchStep
itemStep SAFMeasure = getCount
itemStep SAMacroFMeasure = getClassesInvolved
itemStep SASoftFMeasure = getSoftCounts
itemStep SAFLCFMeasure = getFragCounts
itemStep SAProbabilisticMultiLabelFMeasure = getProbabilisticCounts
itemStep SAProbabilisticSoftFMeasure = getProbabilisticCounts
itemStep SAProbabilisticSoft2DFMeasure = getProbabilisticCounts
itemStep SASoft2DFMeasure = getSoft2DCounts
itemStep SANMI = id
itemStep SALogLossHashed = id
itemStep SALikelihoodHashed = id
itemStep SAPerplexityHashed = id
itemStep SACharMatch = id
itemStep SAMAP = uncurry calculateMAPForOneResult
itemStep SALogLoss = itemLogLossError
itemStep SALikelihood = itemLogLossError
itemStep SABIOF1 = uncurry gatherCountsForBIO
itemStep SABIOWeightedF1 = uncurry gatherSeparatedCountsForBIO
itemStep SABIOF1Labels = uncurry gatherCountsForBIO
itemStep SATokenAccuracy = countHitsAndTotals
itemStep SASegmentAccuracy = uncurry segmentAccuracy
itemStep SAMAE = itemAbsoluteError
itemStep SASMAPE = smape
itemStep (SAMultiLabelFMeasure smatchSpec) = getWeightedCounts (getMatchingFunctionForText $ fromSing smatchSpec)
itemStep SAMultiLabelLogLoss = uncurry countLogLossOnProbList
itemStep SAMultiLabelLikelihood = uncurry countLogLossOnProbList
itemStep SAHaversine = haversine
itemStep SAImprovement = id


doubleParser :: Text -> Either String Double
doubleParser = getValue . TR.double

intoWords :: Text -> Either a [Text]
intoWords = Right . Data.Text.words

intoStringWords :: Text -> Either a [String]
intoStringWords = Right . Prelude.words . unpack

alternativeSentencesParser :: Text -> Either a [[String]]
alternativeSentencesParser = Right . map Prelude.words . DLS.splitOn "\t" . unpack

onlyStrip :: Text -> Either a Text
onlyStrip = Right . strip

justStrip :: Text -> Either a (Maybe Text)
justStrip = Right . Just . strip

predictedParser :: Text -> Maybe Text
predictedParser got =
  -- first try to parse what we got as a probability distribution
  -- (like the one used for Likelikehood/LogLossHashed metric)
  case parseWordSpecs got of
    Right wordSpecs -> if Prelude.null pairs
                      then Nothing
                      else Just $ snd $ maximum pairs
      where pairs = catMaybes $ map wordSpecToPair wordSpecs
    Left _ -> Just got

splitByTabs :: Text -> Either a [[Char]]
splitByTabs = Right . DLS.splitOn "\t" . unpack

zeroOneParser :: Text -> Either String Bool
zeroOneParser = expected <=< (getValue . TR.decimal)
  where expected 1 = Right True
        expected 0 = Right False
        expected _ = Left "expected 0 or 1"

probToZeroOneParser :: Text -> Either String Bool
probToZeroOneParser = detected <=< (getValue . TR.double)
  where -- output value could be a probability (for compatibility with other measures)
        detected prob
          | prob >= 0.0 && prob < detectionThreshold = Right False
          | prob >= detectionThreshold && prob <= 1.0 = Right True
          | otherwise = Left "expected probability"
        detectionThreshold = 0.5

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

smape :: (Double, Double) -> Double
smape (exp, out) = (abs (exp-out)) `safeDoubleDiv` ((abs exp) + (abs out))

hitOrMiss :: (Text, Text) -> Double
hitOrMiss (exp, got) =
  -- first try to parse what we got as a probability distribution
  -- (like the one used for Likelikehood/LogLossHashed metric)
  case parseWordSpecs got of
    Right wordSpecs -> if Prelude.null pairs
                      then 0.0
                      else indicator (exp == (snd $ maximum pairs))
      where pairs = catMaybes $ map wordSpecToPair wordSpecs
    Left _ ->  indicator ((normalizeProbForAccuracy exp got) == exp)
              -- if the expected value is 0 or 1 treat values
              -- between 0.0 and 1.0 as probabilities
              -- for the positive outcome
  where  normalizeProbForAccuracy :: Text -> Text -> Text
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

getCount :: (Bool, Bool) -> (Int, Int, Int)
getCount (True, True)   = (1, 1, 1)
getCount (True, False)  = (0, 1, 0)
getCount (False, True)  = (0, 0, 1)
getCount (False, False) = (0, 0, 0)

getClassesInvolved :: Eq a => (Maybe a, Maybe a) -> (Maybe a, Maybe a, Maybe a)
getClassesInvolved (Just a, Nothing) = (Nothing, Just a, Nothing)
getClassesInvolved (Nothing, Just b) = (Nothing, Nothing, Just b) -- should not occur, for completeness
getClassesInvolved (Just a, Just b) = if a == b
                                      then (Just a, Just a, Just a)
                                      else (Nothing, Just a, Just b)

getWeightedCounts :: (a -> b -> Double) -> ([a], [b]) -> (Double, Int, Int)
getWeightedCounts matchFun (expected, got) = (weightedMaxMatch matchFun expected got,
                                              Prelude.length expected,
                                              Prelude.length got)

getSoftCounts :: EntityWithProbability e => ([BareEntity e], [e]) -> (Double, Int, Int)
getSoftCounts args = getWeightedCounts matchScore args

getSoft2DCounts :: ([LabeledClipping], [ObtainedLabeledClipping]) -> (Integer, Integer, Integer)
getSoft2DCounts (expected, gotWithProbs) = (tpArea, expArea, gotArea)
  where tpArea = coveredBy expected got
        expArea = totalArea expected
        gotArea = totalArea got
        got = map (\(ObtainedLabeledClipping clip _) -> clip)
              $ filter (\(ObtainedLabeledClipping _ prob) -> getP prob >= detectionThreshold) gotWithProbs
        detectionThreshold = 0.5

getFragCounts :: CoverableEntityWithProbability e => ([BareEntity e], [e]) -> (Double, Double, Int, Int)
getFragCounts (expected, got)
  | allDisjoint (map getBareEntity got) = (
      recallScoreTotal expected got,
      precisionScoreTotal got expected,
      Prelude.length expected,
      Prelude.length got)
  | otherwise = (0, 0, Prelude.length expected, Prelude.length got)

countHitsAndTotals :: ([Text], [Text]) -> (Int, Int)
countHitsAndTotals (es, os) =
  if Prelude.length os /= Prelude.length es
  then throw $ OtherException "wrong number of tokens"
  else Prelude.foldl matchFun
                     (0, 0)
                     (zip es os)
  where  matchFun :: (Int, Int) -> (Text, Text) -> (Int, Int)
         matchFun (h, t) (e, o)
           | e == (pack "*") = (h, t)
           | o `Prelude.elem` (splitOn (pack ";") e) = (h + 1, t + 1)
           | otherwise = (h, t + 1)

parseSpherePoints :: Text -> Either String (Double, Double)
parseSpherePoints t = case DLS.splitOn "\t" (unpack t) of
   [longitudeStr, latitudeStr] -> case doubleParser (pack longitudeStr) of
     Right longitude -> case doubleParser (pack latitudeStr) of
       Right latitude -> Right (longitude, latitude)
       Left _ -> Left "cannot parse line with latitude of sphere"
     Left _ -> Left "cannot parse line with longitude of sphere"
   _ -> Left "cannot parse line with longitude and latitude of sphere"
