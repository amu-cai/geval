{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GEval.MetricsMeta
  (listOfAvailableMetrics,
   listOfAvailableEvaluationSchemes,
   extraInfo,
   isEvaluationSchemeDescribed,
   getEvaluationSchemeDescription,
   outContents,
   expectedScore,
   allMetricsDescription)
  where

import GEval.Common
import GEval.Metric
import GEval.EvaluationScheme
import GEval.CreateChallenge (testExpectedContents)
import GEval.PrecisionRecall (weightedHarmonicMean)

import Text.Regex.PCRE.Heavy
import Data.Either (fromRight)
import Data.String.Here
import Data.Maybe (fromMaybe)

import Data.List (intercalate)
import Text.Printf

-- | the list of available metrics, to be shown to the user or to be
-- | used for tests
listOfAvailableMetrics :: [Metric]
listOfAvailableMetrics = [RMSE,
                          MSE,
                          MAE,
                          SMAPE,
                          Pearson,
                          Spearman,
                          Accuracy,
                          LogLoss,
                          Likelihood,
                          FMeasure 1.0,
                          FMeasure 2.0,
                          FMeasure 0.25,
                          MacroFMeasure 1.0,
                          MacroFMeasure 2.0,
                          MacroFMeasure 0.25,
                          MultiLabelFMeasure 1.0,
                          MultiLabelFMeasure 2.0,
                          MultiLabelFMeasure 0.25,
                          MultiLabelLikelihood,
                          MAP,
                          BLEU,
                          GLEU,
                          WER,
                          NMI,
                          ClippEU,
                          LogLossHashed defaultLogLossHashedSize,
                          LikelihoodHashed defaultLogLossHashedSize,
                          BIOF1,
                          BIOF1Labels,
                          TokenAccuracy,
                          SoftFMeasure 1.0,
                          SoftFMeasure 2.0,
                          SoftFMeasure 0.25,
                          ProbabilisticSoftFMeasure 1.0,
                          ProbabilisticSoftFMeasure 2.0,
                          ProbabilisticSoftFMeasure 0.25,
                          Soft2DFMeasure 1.0,
                          Soft2DFMeasure 2.0,
                          Soft2DFMeasure 0.25,
                          CharMatch]

extraInfo :: EvaluationScheme -> Maybe String
extraInfo (EvaluationScheme GLEU [])  = Just "\"Google GLEU\" not the grammar correction metric"
extraInfo (EvaluationScheme BLEU [LowerCasing,
                                 RegexpMatch _]) = Just "BLEU on lowercased strings, only Latin characters and digits considered"
extraInfo _ = Nothing

-- As we just started describing metrics (or, to be precise,
-- evaluation schemes), we need keep track of which metric is
-- described and which - not.
-- When all the metrics are described, this function should be
-- removed.
isEvaluationSchemeDescribed :: EvaluationScheme -> Bool
isEvaluationSchemeDescribed (EvaluationScheme metric []) = isMetricDescribed metric
isEvaluationSchemeDescribed _ = False

isMetricDescribed :: Metric -> Bool
isMetricDescribed (SoftFMeasure _) = True
isMetricDescribed (Soft2DFMeasure _) = True
isMetricDescribed _ = False

getEvaluationSchemeDescription :: EvaluationScheme -> String
getEvaluationSchemeDescription (EvaluationScheme metric []) = getMetricDescription metric

getMetricDescription :: Metric -> String
getMetricDescription (SoftFMeasure _) =
  [i|"Soft" F-measure on intervals, i.e. partial "hits" are considered. For instance,
if a label `foo` is expected for the span 2-9 and this label is returned but with
the span 8-12, it is counted as 2/8=0.25 instead of 0 or 1 when precision/recall counts
are gathered.
|]
getMetricDescription (Soft2DFMeasure _) =
  [i|"Soft" F-measure on rectangles, i.e. precision and recall is calculated for areas. For instance,
if a label `foo` is expected for the rectangle (0, 0)-(100, 200) and this label is returned but with
the span (50, 100)-(150, 150), it is treated as recall=1/8 and precision=1/2. For each item (line) F-score
is evaluated separately and finally averaged.
|]

outContents :: Metric -> String
outContents (SoftFMeasure _) = [hereLit|inwords:1-4
inwords:1-3 indigits:5
|]
outContents (Soft2DFMeasure _) = [hereLit|foo:3/250,130,340,217
bar:1/0,0,100,200 foo:1/40,50,1000,1000 bar:1/400,600,1000,1000
|]

expectedScore :: EvaluationScheme -> MetricValue
expectedScore (EvaluationScheme (SoftFMeasure beta) [])
  = let precision = 0.25
        recall = 0.75
      in weightedHarmonicMean beta precision recall
expectedScore (EvaluationScheme (Soft2DFMeasure beta) [])
  = let precision = 0.211622914314256
        recall = 0.2749908502976
      in (weightedHarmonicMean beta precision recall) / 2.0

listOfAvailableEvaluationSchemes :: [EvaluationScheme]
listOfAvailableEvaluationSchemes = map (\m -> EvaluationScheme m []) listOfAvailableMetrics
                                   ++ [
                                   EvaluationScheme BLEU [LowerCasing,
                                                          RegexpMatch (fromRight undefined $ compileM "\\s+|[a-z0-9]+" [])]
                                   ]

allMetricsDescription :: String
allMetricsDescription =
  intercalate "\n\n\n" $ map formatEvaluationSchemeDescription listOfAvailableEvaluationSchemes

formatEvaluationSchemeDescription :: EvaluationScheme -> String
formatEvaluationSchemeDescription scheme@(EvaluationScheme metric _) = show scheme ++ "\n" ++ description
  where description = if isEvaluationSchemeDescribed scheme
                      then (getEvaluationSchemeDescription scheme)
                           ++ "\n"
                           ++ (formatDescription metric)
                           ++ "\nExample\n"
                           ++ (pasteLines "Expected output" "Sample output")
                           ++ concat (map (\(exp, out) -> pasteLines exp out) $ zip (lines $ testExpectedContents metric)
                                                                                   (lines $ outContents metric))
                           ++ "\nMetric value: " ++ (printf "%.4f" $ expectedScore scheme)
                           ++ (case scoreExplanation scheme of
                                 Just expl -> "\n(" ++ expl ++ ")"
                                 Nothing -> "")
                      else noDescription
        noDescription = [hereLit|THE METRIC HAS NO DESCRIPTION YET, PLEASE ADD AN ISSUE TO https://gitlab.com/filipg/geval/issues
IF YOU WANT TO HAVE IT DESCRIBED|]

formatDescription :: Metric -> String
formatDescription (SoftFMeasure _) = [hereLit|Each line is a sequence of entities separated by spaces, each entity is of
the form LABEL:SPAN, where LABEL is any label and SPAN is defined using single integers, intervals or such
units separated with commas.
|]
formatDescription (Soft2DFMeasure _) = [hereLit|Each line is a sequence of entities separated by spaces, each entity is of
the form LABEL:PAGE/X0,Y0,X1,Y1 where LABEL is any label, page is the page number (starting from 1) and
(X0, Y0) and (X1, Y1) are clipping corners.
|]

scoreExplanation :: EvaluationScheme -> Maybe String
scoreExplanation (EvaluationScheme (SoftFMeasure _) [])
  = Just [hereLit|We have a partial (0.75) success for the entity `inwords:1-4`, hence Recall = 0.75/1 = 0.75,
Precision = (0 + 0.75 + 0) / 3 = 0.25, so F-score = 0.375|]
scoreExplanation (EvaluationScheme (Soft2DFMeasure _) [])
  = Just [hereLit|The F-score for the first item is 0 (the entity was found in the completely wrong place).
As far as the second item is concerned, the total area that covered by the output is 50*150+600*400=247500.
Hence, recall is 247500/902500=0.274 and precision - 247500/(20000+912000+240000)=0.211. Therefore, the F-score
for the second item is 0.238 and the F-score for the whole set is (0 + 0.238)/2 = 0.119.|]

pasteLines :: String -> String -> String
pasteLines a b = printf "%-35s %s\n" a b
