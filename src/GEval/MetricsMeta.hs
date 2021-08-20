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
   allMetricsDescription,
   helpMetricParameterMetricsList)
  where

import GEval.Common
import GEval.Metric
import GEval.EvaluationScheme
import GEval.CreateChallenge (testExpectedContents)
import GEval.PrecisionRecall (weightedHarmonicMean)
import GEval.MatchingSpecification (MatchingSpecification(ExactMatch))

import Text.Regex.PCRE.Heavy
import Data.Either (fromRight)
import Data.String.Here

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
                          Accuracy ExactMatch,
                          LogLoss,
                          Likelihood,
                          FMeasure 1.0,
                          FMeasure 2.0,
                          FMeasure 0.25,
                          MacroFMeasure 1.0,
                          MacroFMeasure 2.0,
                          MacroFMeasure 0.25,
                          MultiLabelFMeasure 1.0 ExactMatch,
                          MultiLabelFMeasure 2.0 ExactMatch,
                          MultiLabelFMeasure 0.25 ExactMatch,
                          Mean (MultiLabelFMeasure 1.0 ExactMatch),
                          ProbabilisticMultiLabelFMeasure 1.0,
                          ProbabilisticMultiLabelFMeasure 2.0,
                          ProbabilisticMultiLabelFMeasure 0.25,
                          MultiLabelLikelihood,
                          MAP,
                          BLEU,
                          GLEU,
                          WER,
                          CER,
                          NMI,
                          ClippEU,
                          LogLossHashed defaultLogLossHashedSize,
                          LikelihoodHashed defaultLogLossHashedSize,
                          PerplexityHashed defaultLogLossHashedSize,
                          BIOF1,
                          BIOWeightedF1,
                          BIOF1Labels,
                          TokenAccuracy,
                          SegmentAccuracy,
                          SoftFMeasure 1.0,
                          SoftFMeasure 2.0,
                          SoftFMeasure 0.25,
                          ProbabilisticSoftFMeasure 1.0,
                          ProbabilisticSoftFMeasure 2.0,
                          ProbabilisticSoftFMeasure 0.25,
                          Soft2DFMeasure 1.0,
                          Soft2DFMeasure 2.0,
                          Soft2DFMeasure 0.25,
                          Haversine,
                          CharMatch]

extraInfo :: EvaluationScheme -> Maybe String
extraInfo (EvaluationScheme CER []) = Just "Character-Error Rate"
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
isMetricDescribed (MultiLabelFMeasure 1.0 ExactMatch) = True
isMetricDescribed (SoftFMeasure _) = True
isMetricDescribed (Soft2DFMeasure _) = True
isMetricDescribed (ProbabilisticMultiLabelFMeasure _) = True
isMetricDescribed GLEU = True
isMetricDescribed WER = True
isMetricDescribed CER = True
isMetricDescribed SegmentAccuracy = True
isMetricDescribed Haversine = True
isMetricDescribed BIOWeightedF1 = True
isMetricDescribed _ = False

getEvaluationSchemeDescription :: EvaluationScheme -> String
getEvaluationSchemeDescription (EvaluationScheme metric []) = getMetricDescription metric

getMetricDescription :: Metric -> String
getMetricDescription (MultiLabelFMeasure 1.0 ExactMatch) =
  [i|F-measure (or F-score), i.e. harmonic mean of the precision and
recall. It can be calculated for any labels. Precision is the fraction
of correct labels among the ones in the output, whereas recall is the
fraction of instances in the gold standard that were correctly
retrieved. Counts for precision and recall are calculated for the
whole test set, in other words it is a micro-average (it is NOT the
case that F-scores are calculated for each document or label class
separately and then averaged). For a macro-average per document, see
Mean/Multilabel-F1.
|]
getMetricDescription (SoftFMeasure _) =
  [i|"Soft" F-measure on intervals, i.e. partial "hits" are considered. For instance,
if a label `foo` is expected for the span 2-9 and this label is returned, but with
the span 8-12, it is counted as 2/8=0.25 instead of 0 or 1 when precision/recall counts
are gathered.
|]
getMetricDescription (Soft2DFMeasure _) =
  [i|"Soft" F-measure on rectangles, i.e. precision and recall is calculated for areas. For instance,
if a label `foo` is expected for the rectangle (0, 0)-(100, 200) and this label is returned but with
the span (50, 100)-(150, 150), it is treated as recall=1/8 and precision=1/2. For each item (line) F-score
is evaluated separately and finally averaged.
|]
getMetricDescription (ProbabilisticMultiLabelFMeasure _) =
  [i|F-measure generalised so that labels could annotated with probabilities and the quality
of probabilities is assessed as well. It is calculated as the harmonic mean of calibration and recall
where calibration measures the quality of probabilities (how well they are calibrated, e.g.
if we have 10 items with probability 0.5 and 5 of them are correct, then the calibration
is perfect.
|]
getMetricDescription GLEU =
  [i|For the GLEU score, we record all sub-sequences of
1, 2, 3 or 4 tokens in output and target sequence (n-grams). We then
compute a recall, which is the ratio of the number of matching n-grams
to the number of total n-grams in the target (ground truth) sequence,
and a precision, which is the ratio of the number of matching n-grams
to the number of total n-grams in the generated output sequence. Then
GLEU score is simply the minimum of recall and precision. This GLEU
score's range is always between 0 (no matches) and 1 (all match) and
it is symmetrical when switching output and target. According to
the article, GLEU score correlates quite well with the BLEU
metric on a corpus level but does not have its drawbacks for our per
sentence reward objective.
see: https://arxiv.org/pdf/1609.08144.pdf
|]
getMetricDescription WER =
  [i|WER (Word-Error Rate) is the number of word-level mistakes divided
by the number of words in the expected output. Possible mistakes are
deletions, insertions and substitions — as in the Levenshtein distance.
|]
getMetricDescription CER =
  [i|CER (Character-Error Rate) is the number of character-level mistakes divided
by the total length of the expected output. Possible mistakes are
deletions, insertions and substitions — as in the Levenshtein distance.
|]

getMetricDescription SegmentAccuracy =
  [i|Accuracy counted for segments, i.e. labels with positions.
The percentage of labels in the ground truth retrieved in the actual output is returned.
Accuracy is calculated separately for each item and then averaged.
|]
getMetricDescription Haversine =
  [i|The haversine formula determines the great-circle distance between
two points on a sphere given their longitudes and latitudes (in degrees).
|]
getMetricDescription BIOWeightedF1 =
  [i|Weighted-average F1-score calculated on output expressed in the BIO format.
|]

outContents :: Metric -> String
outContents (MultiLabelFMeasure _ _) = [hereLit|person/1,3 first-name/1:0.8 first-name/3:0.75
surname/2 county/1:0.33
first-name/3:0.52
|]
outContents (SoftFMeasure _) = [hereLit|inwords:1-4
inwords:1-3 indigits:5
|]
outContents (Soft2DFMeasure _) = [hereLit|foo:3/250,130,340,217
bar:1/0,0,100,200 foo:1/40,50,1000,1000 bar:1/400,600,1000,1000
|]
outContents (ProbabilisticMultiLabelFMeasure _) = [hereLit|first-name/1:0.8 surname/3:1.0
surname/1:0.4
first-name/3:0.9
|]
outContents GLEU = [hereLit|Alice has a black
|]
outContents WER = [hereLit|na ka huainaua e te atua te marama ko te awatea , a ko te pouri i huaina e ia ko te po
a ko te ahiahi , ko ata , he ra ko kotahi
|]
outContents CER = [hereLit|esse esi perctp
tabula rasai
|]
outContents SegmentAccuracy = [hereLit|N:1-4 V:5-6 N:8-10 V:12-13 A:15-17
N:1-4 V:6-7 A:9-13
|]
outContents Haversine = [hereLit|39.575264	-56.995928
29.949932	-90.070116
|]
outContents BIOWeightedF1 = [hereLit|B-firstname/ALAN B-surname/TURING
O O O
B-surname/TARSKI O B-surname/NOT O
|]

expectedScore :: EvaluationScheme -> MetricValue
expectedScore (EvaluationScheme (MultiLabelFMeasure 1.0 ExactMatch) []) = 0.6666
expectedScore (EvaluationScheme (SoftFMeasure beta) [])
  = let precision = 0.25
        recall = 0.75
      in weightedHarmonicMean beta precision recall
expectedScore (EvaluationScheme (Soft2DFMeasure beta) [])
  = let precision = 0.211622914314256
        recall = 0.2749908502976
      in (weightedHarmonicMean beta precision recall) / 2.0
expectedScore (EvaluationScheme (ProbabilisticMultiLabelFMeasure beta) [])
  = let precision = 0.6569596940847289
        recall = 0.675
      in weightedHarmonicMean beta precision recall
expectedScore (EvaluationScheme GLEU [])
  = 0.7142857142857143
expectedScore (EvaluationScheme SegmentAccuracy [])
  = 0.875
expectedScore (EvaluationScheme WER [])
  = 0.08571
expectedScore (EvaluationScheme CER [])
  = 0.14814
expectedScore (EvaluationScheme Haversine [])
  = 1044.2633358563135
expectedScore (EvaluationScheme BIOWeightedF1 [])
  = 0.86666666

helpMetricParameterMetricsList :: String
helpMetricParameterMetricsList = intercalate ", " $ map (\s -> (show s) ++ (case extraInfo s of
                                                                             Just eI -> " (" ++ eI ++ ")"
                                                                             Nothing -> ""))
                                                    listOfAvailableEvaluationSchemes

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
formatDescription (MultiLabelFMeasure _ ExactMatch) = [hereLit|Any label separated by spaces can be used. Labels are not
interpreted except that they can be accompanied by probabilities
(after a colon): only labels with probabilities >= 0.5 are considered.
This is for compatibility with probalistic metrics. By default, 1.0 is
assumed as the probability, but it is recommended to add probabilities
explicitly.
|]
formatDescription (SoftFMeasure _) = [hereLit|Each line is a sequence of entities separated by spaces, each entity is of
the form LABEL:SPAN, where LABEL is any label and SPAN is defined using single integers, intervals or such
units separated with commas.
|]
formatDescription (Soft2DFMeasure _) = [hereLit|Each line is a sequence of entities separated by spaces, each entity is of
the form LABEL:PAGE/X0,Y0,X1,Y1 where LABEL is any label, page is the page number (starting from 1) and
(X0, Y0) and (X1, Y1) are clipping corners.
|]
formatDescription (ProbabilisticMultiLabelFMeasure _) = [hereLit|In each line a number of labels (entities) can be given. A label probability
can be provided with a colon (e.g. "foo:0.7"). By default, 1.0 is assumed.
|]
formatDescription GLEU = [hereLit|In each line a there is a space sparated sequence of words.
|]
formatDescription SegmentAccuracy = [hereLit|Labels can be any strings (without spaces), whereas is a list of
1-based indexes or spans separated by commas (spans are inclusive
ranges, e.g. "10-14"). For instance, "foo:bar:2,4-7,10" is a
label "foo:bar" for positions 2, 4, 5, 6, 7 and 10. Note that no
overlapping segments can be returned (evaluation will fail in
such a case).
|]
formatDescription WER = formatDescription GLEU
formatDescription CER = [hereLit|Any text, whitespace and punctuation marks are also considered.
|]
formatDescription Haversine = [hereLit|Each line is a latitude and longitude of sphere separated by tabulation,
e.g. "41.558153 -73.051497".
|]
formatDescription BIOWeightedF1 = [hereLit|Each line is a sequence of tags encoded in the BIO format, i.e. O, B-tag, I-tag;
B-tags and I-tags can accompanied by an extra label after a slash.
|]


scoreExplanation :: EvaluationScheme -> Maybe String
scoreExplanation (EvaluationScheme (MultiLabelFMeasure _ ExactMatch) [])
  = Just [hereLit|Out of the total 5 labels in the output, 3 are correct (person/1,3, first-name/1 and
first-name/3, only labels with probabilities >= 0.5 are considered, otherwise the probabilities are just
discarded), hence precision is 3/5=0.6, whereas out of the 4 labels in gold standard, again 3 were
retrieved, so recall is 3/4=0.75. The harmonic mean of precision and recall is 2/(4/3 + 5/3) =
= 2/3 = 0.6666|]
scoreExplanation (EvaluationScheme (SoftFMeasure _) [])
  = Just [hereLit|We have a partial (0.75) success for the entity `inwords:1-4`, hence Recall = 0.75/1 = 0.75,
Precision = (0 + 0.75 + 0) / 3 = 0.25, so F-score = 0.375|]
scoreExplanation (EvaluationScheme (Soft2DFMeasure _) [])
  = Just [hereLit|The F-score for the first item is 0 (the entity was found in the completely wrong place).
As far as the second item is concerned, the total area that covered by the output is 50*150+600*400=247500.
Hence, recall is 247500/902500=0.274 and precision - 247500/(20000+912000+240000)=0.211. Therefore, the F-score
for the second item is 0.238 and the F-score for the whole set is (0 + 0.238)/2 = 0.119.|]
scoreExplanation (EvaluationScheme (ProbabilisticMultiLabelFMeasure _) []) = Nothing
scoreExplanation (EvaluationScheme GLEU [])
  = Just [hereLit|To find out GLEU score we first count number of tp (true positives) fp(false positives) and fn(false negatives).
  We have 4 matching unigrams ("Alice", "has", "a", "black") , 3 bigrams ("Alice has", "has a", "a black"), 2 trigrams ("Alice has a", "has a black") and 1 tetragram ("Alice has a black"),
so tp=10. We have no fp, therefore fp=0. There are 4 fn - ("cat", "black cat", "a black cat", "has a black cat").
Now we have to calculate precision and recall:
  Precision is tp / (tp+fp) = 10/(10+0) = 1,
  recall is tp / (tp+fn) = 10 / (10+4) = 10/14 =~ 0.71428...
  The GLEU score is min(precision,recall)=0.71428 |]
scoreExplanation (EvaluationScheme SegmentAccuracy [])
  = Just [hereLit|Out of 4 segments in the expected output for the first item, 3 were retrieved correcly (accuracy is 3/4=0.75).
The second item was retrieved perfectly (accuracy is 1.0). Hence, the average is (0.75+1.0)/2=0.875.|]
scoreExplanation (EvaluationScheme WER [])
  = Just [hereLit|The total length of expected output (in words) is 35. There are 3 errors
(1 word substituted, 1 inserted, 1 deleted)  in the actual output. Hence,
WER = (1+1+1) / 35 = 3 / 35 = 0.08571.|]
scoreExplanation (EvaluationScheme CER [])
  = Just [hereLit|The total length of expected output (in characters) is 27. There are 4 errors
(1 word substituted, 1 inserted, 1 deleted)  in the actual output. Hence,
CER = (2+1+1) / 27 = 4 / 27 = 0.14814.|]
scoreExplanation (EvaluationScheme Haversine []) = Nothing
scoreExplanation (EvaluationScheme BIOWeightedF1 [])
  = Just [hereLit|There are two labels (firstname and surname, O is not considered). Firstname was
predicted in the perfect way, hence F1=1, whereas for surname recall is 1, precision - 2/3 and F1 - 4/5.
The weighted average is (1 * 1 + 2 * 4/5) / 3 = 13/15 = 0.8667.|]

pasteLines :: String -> String -> String
pasteLines a b = printf "%-35s %s\n" a b
