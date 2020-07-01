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
import GEval.Clippings (totalArea, coveredBy, clippEUMatchStep)
import GEval.BIO (gatherCountsForBIO)

import GEval.Probability
import GEval.PrecisionRecall (weightedMaxMatch, fMeasureOnCounts, calculateMAPForOneResult, getProbabilisticCounts, getCounts)

import Control.Exception

import Data.Text
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
import GEval.ProbList (ProbList(..), parseIntoProbList, WordWithProb(..), countLogLossOnProbList)
import GEval.MatchingSpecification

-- | Helper type so that singleton can be used.
-- | (The problem is that some metrics are parametrized by Double
-- | Word32 and this is not handled by the singleton libary.)
singletons [d|data AMetric = ARMSE | AMSE | APearson | ASpearman | ABLEU | AGLEU | AWER | AAccuracy | AClippEU
                             | AFMeasure | AMacroFMeasure | ANMI
                             | ALogLossHashed | ACharMatch | AMAP | ALogLoss | ALikelihood
                             | ABIOF1 | ABIOF1Labels | ATokenAccuracy | ASegmentAccuracy | ALikelihoodHashed | AMAE | ASMAPE | AMultiLabelFMeasure MatchingSpecification
                             | AMultiLabelLogLoss | AMultiLabelLikelihood
                             | ASoftFMeasure | AProbabilisticMultiLabelFMeasure | AProbabilisticSoftFMeasure | ASoft2DFMeasure
                             | AFLCFMeasure
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
toHelper Accuracy = AAccuracy
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
toHelper BIOF1Labels = ABIOF1Labels
toHelper TokenAccuracy = ATokenAccuracy
toHelper SegmentAccuracy = ASegmentAccuracy
toHelper (LikelihoodHashed _) = ALikelihoodHashed
toHelper MAE = AMAE
toHelper SMAPE = ASMAPE
toHelper (MultiLabelFMeasure _ matchingSpec) = AMultiLabelFMeasure matchingSpec
toHelper MultiLabelLogLoss = AMultiLabelLogLoss
toHelper MultiLabelLikelihood = AMultiLabelLikelihood
toHelper (SoftFMeasure _) = ASoftFMeasure
toHelper (FLCFMeasure _) = AFLCFMeasure
toHelper (ProbabilisticMultiLabelFMeasure _) = AProbabilisticMultiLabelFMeasure
toHelper (ProbabilisticSoftFMeasure _) = AProbabilisticSoftFMeasure
toHelper (Soft2DFMeasure _) = ASoft2DFMeasure

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
  ParsedExpectedType AAccuracy = Text
  ParsedExpectedType AClippEU  = [ClippingSpec]
  ParsedExpectedType AFMeasure = Bool
  ParsedExpectedType AMacroFMeasure = Maybe Text
  ParsedExpectedType ASoftFMeasure = [Annotation]
  ParsedExpectedType AFLCFMeasure = [Annotation]
  ParsedExpectedType AProbabilisticMultiLabelFMeasure = [Text]
  ParsedExpectedType AProbabilisticSoftFMeasure = [Annotation]
  ParsedExpectedType ASoft2DFMeasure = [LabeledClipping]
  ParsedExpectedType ANMI = Text
  ParsedExpectedType ALogLossHashed = Text
  ParsedExpectedType ALikelihoodHashed = Text
  ParsedExpectedType ACharMatch = Text
  ParsedExpectedType AMAP = [String]
  ParsedExpectedType ALogLoss = Double
  ParsedExpectedType ALikelihood = Double
  ParsedExpectedType ABIOF1 = [TaggedEntity]
  ParsedExpectedType ABIOF1Labels = [TaggedEntity]
  ParsedExpectedType ATokenAccuracy = [Text]
  ParsedExpectedType ASegmentAccuracy = [Annotation]
  ParsedExpectedType AMAE = Double
  ParsedExpectedType ASMAPE = Double
  ParsedExpectedType (AMultiLabelFMeasure _) = [Text]
  ParsedExpectedType AMultiLabelLogLoss = [Text]
  ParsedExpectedType AMultiLabelLikelihood = [Text]

expectedParser :: SAMetric t -> Text -> Either String (ParsedExpectedType t)
expectedParser SARMSE = doubleParser
expectedParser SAMSE = doubleParser
expectedParser SAPearson = doubleParser
expectedParser SASpearman = doubleParser
expectedParser SABLEU = alternativeSentencesParser
expectedParser SAGLEU = alternativeSentencesParser
expectedParser SAWER = intoStringWords
expectedParser SAAccuracy = onlyStrip
expectedParser SAClippEU = controlledParse lineClippingSpecsParser
expectedParser SAFMeasure = zeroOneParser
expectedParser SAMacroFMeasure = justStrip
expectedParser SASoftFMeasure = parseAnnotations
expectedParser SAFLCFMeasure = parseAnnotations
expectedParser SAProbabilisticMultiLabelFMeasure = intoWords
expectedParser SAProbabilisticSoftFMeasure = parseAnnotations
expectedParser SASoft2DFMeasure = controlledParse lineLabeledClippingsParser
expectedParser SANMI = Right . id
expectedParser SALogLossHashed = onlyStrip
expectedParser SALikelihoodHashed = onlyStrip
expectedParser SACharMatch = Right
expectedParser SAMAP = splitByTabs
expectedParser SALogLoss = doubleParser
expectedParser SALikelihood = doubleParser
expectedParser SABIOF1 = parseBioSequenceIntoEntities
expectedParser SABIOF1Labels = parseBioSequenceIntoEntitiesWithoutNormalization
expectedParser SATokenAccuracy = intoWords
expectedParser SASegmentAccuracy = parseSegmentAnnotations
expectedParser SAMAE = doubleParser
expectedParser SASMAPE = doubleParser
expectedParser (SAMultiLabelFMeasure _) = intoWords
expectedParser SAMultiLabelLogLoss = intoWords
expectedParser SAMultiLabelLikelihood = intoWords

type family ParsedOutputType (t :: AMetric) :: * where
  ParsedOutputType ABLEU = [String]
  ParsedOutputType AGLEU = [String]
  ParsedOutputType AClippEU = [Clipping]
  ParsedOutputType AMacroFMeasure = Maybe Text
  ParsedOutputType ASoftFMeasure = [ObtainedAnnotation]
  ParsedOutputType AFLCFMeasure = [ObtainedAnnotation]
  ParsedOutputType AProbabilisticSoftFMeasure = [ObtainedAnnotation]
  ParsedOutputType AProbabilisticMultiLabelFMeasure = [WordWithProb]
  ParsedOutputType AMultiLabelLikelihood = ProbList
  ParsedOutputType AMultiLabelLogLoss = ProbList
  ParsedOutputType t = ParsedExpectedType t

outputParser :: SAMetric t -> Text -> Either String (ParsedOutputType t)
outputParser SARMSE = expectedParser SARMSE
outputParser SAMSE = expectedParser SARMSE
outputParser SAPearson = expectedParser SAPearson
outputParser SASpearman = expectedParser SASpearman
outputParser SABLEU = Right . Prelude.words . unpack
outputParser SAGLEU = Right . Prelude.words . unpack
outputParser SAWER = expectedParser SAWER
outputParser SAAccuracy = expectedParser SAAccuracy
outputParser SAClippEU = controlledParse lineClippingsParser
outputParser SAFMeasure = probToZeroOneParser
outputParser SAMacroFMeasure = Right . predictedParser . strip
outputParser SASoftFMeasure = parseObtainedAnnotations
outputParser SAFLCFMeasure = parseObtainedAnnotations
outputParser SAProbabilisticMultiLabelFMeasure = (Right . (\(ProbList es) -> es) . parseIntoProbList)
outputParser SAProbabilisticSoftFMeasure = parseObtainedAnnotations
outputParser SASoft2DFMeasure = expectedParser SASoft2DFMeasure
outputParser SANMI = expectedParser SANMI
outputParser SALogLossHashed = onlyStrip
outputParser SALikelihoodHashed = onlyStrip
outputParser SACharMatch = Right
outputParser SAMAP = splitByTabs
outputParser SALogLoss = doubleParser
outputParser SALikelihood = doubleParser
outputParser SABIOF1 = parseBioSequenceIntoEntities
outputParser SABIOF1Labels = parseBioSequenceIntoEntitiesWithoutNormalization
outputParser SATokenAccuracy = intoWords
outputParser SASegmentAccuracy = parseSegmentAnnotations
outputParser SAMAE = doubleParser
outputParser SASMAPE = doubleParser
outputParser (SAMultiLabelFMeasure _) = intoWords
outputParser SAMultiLabelLogLoss = Right . parseIntoProbList
outputParser SAMultiLabelLikelihood = Right . parseIntoProbList

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
  ItemIntermediateRepresentationType ABIOF1Labels = (Int, Int, Int)
  ItemIntermediateRepresentationType ATokenAccuracy = (Int, Int)
  ItemIntermediateRepresentationType AProbabilisticMultiLabelFMeasure = ([Double], [Double], Double, Int)
  ItemIntermediateRepresentationType AProbabilisticSoftFMeasure = ([Double], [Double], Double, Int)
  ItemIntermediateRepresentationType APearson = (Double, Double)
  ItemIntermediateRepresentationType ASpearman = (Double, Double)
  -- FIXME
  -- It would be better to distinguish ExactMatch here (for which we could return (Int, Int, Int)
  -- ant other possibilities, but it resulted in too much down-the-rabbit hole with types.
  ItemIntermediateRepresentationType (AMultiLabelFMeasure _) = (Double, Int, Int)
  ItemIntermediateRepresentationType ALogLossHashed = (Text, Text)
  ItemIntermediateRepresentationType ALikelihoodHashed = (Text, Text)
  ItemIntermediateRepresentationType ACharMatch = (Text, Text)
  ItemIntermediateRepresentationType AWER = (Int, Int)
  ItemIntermediateRepresentationType t = Double

itemStep :: SAMetric t -> (ParsedExpectedType t, ParsedOutputType t) -> ItemIntermediateRepresentationType t
itemStep SARMSE = itemSquaredError
itemStep SAMSE = itemSquaredError
itemStep SAPearson = id
itemStep SASpearman = id
itemStep SABLEU = uncurry bleuStep
itemStep SAGLEU = uncurry gleuStep
itemStep SAWER = uncurry werStep
itemStep SAAccuracy = hitOrMiss
itemStep SAClippEU = clippEUMatchStep
itemStep SAFMeasure = getCount
itemStep SAMacroFMeasure = getClassesInvolved
itemStep SASoftFMeasure = getSoftCounts
itemStep SAFLCFMeasure = getFragCounts
itemStep SAProbabilisticMultiLabelFMeasure = getProbabilisticCounts
itemStep SAProbabilisticSoftFMeasure = getProbabilisticCounts
itemStep SASoft2DFMeasure = getSoft2DCounts
itemStep SANMI = id
itemStep SALogLossHashed = id
itemStep SALikelihoodHashed = id
itemStep SACharMatch = id
itemStep SAMAP = uncurry calculateMAPForOneResult
itemStep SALogLoss = itemLogLossError
itemStep SALikelihood = itemLogLossError
itemStep SABIOF1 = uncurry gatherCountsForBIO
itemStep SABIOF1Labels = uncurry gatherCountsForBIO
itemStep SATokenAccuracy = countHitsAndTotals
itemStep SASegmentAccuracy = uncurry segmentAccuracy
itemStep SAMAE = itemAbsoluteError
itemStep SASMAPE = smape
itemStep (SAMultiLabelFMeasure smatchSpec) = getWeightedCounts (getMatchingFunctionForText $ fromSing smatchSpec)
itemStep SAMultiLabelLogLoss = uncurry countLogLossOnProbList
itemStep SAMultiLabelLikelihood = uncurry countLogLossOnProbList


doubleParser = getValue . TR.double

intoWords = Right . Data.Text.words

intoStringWords = Right . Prelude.words . unpack

alternativeSentencesParser = Right . Prelude.map Prelude.words . DLS.splitOn "\t" . unpack

onlyStrip = Right . strip

justStrip = Right . Just . strip

predictedParser got =
  -- first try to parse what we got as a probability distribution
  -- (like the one used for Likelikehood/LogLossHashed metric)
  case parseWordSpecs got of
    Right wordSpecs -> if Prelude.null pairs
                      then Nothing
                      else Just $ snd $ Prelude.maximum pairs
      where pairs = catMaybes $ Prelude.map wordSpecToPair wordSpecs
    Left _ -> Just got

splitByTabs = Right . DLS.splitOn "\t" . unpack

zeroOneParser = expected <=< (getValue . TR.decimal)
  where expected 1 = Right True
        expected 0 = Right False
        expected _ = Left "expected 0 or 1"

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

smape (exp, out) = (abs (exp-out)) `safeDoubleDiv` ((abs exp) + (abs out))

hitOrMiss (exp, got) =
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

getClassesInvolved (Just a, Nothing) = (Nothing, Just a, Nothing)
getClassesInvolved (Nothing, Just b) = (Nothing, Nothing, Just b) -- should not occur, for completeness
getClassesInvolved (Just a, Just b) = if a == b
                                      then (Just a, Just a, Just a)
                                      else (Nothing, Just a, Just b)

getWeightedCounts :: (a -> b -> Double) -> ([a], [b]) -> (Double, Int, Int)
getWeightedCounts matchFun (expected, got) = (weightedMaxMatch matchFun expected got,
                                              Prelude.length expected,
                                              Prelude.length got)

getSoftCounts args = getWeightedCounts matchScore args

getSoft2DCounts (expected, got) = (tpArea, expArea, gotArea)
  where tpArea = coveredBy expected got
        expArea = totalArea expected
        gotArea = totalArea got

getFragCounts :: CoverableEntityWithProbability e => ([BareEntity e], [e]) -> (Double, Double, Int, Int)
getFragCounts (expected, got)
  | allDisjoint (Prelude.map getBareEntity got) = (
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
                     (Prelude.zip es os)
  where  matchFun :: (Int, Int) -> (Text, Text) -> (Int, Int)
         matchFun (h, t) (e, o)
           | e == (pack "*") = (h, t)
           | o `Prelude.elem` (splitOn (pack ";") e) = (h + 1, t + 1)
           | otherwise = (h, t + 1)
