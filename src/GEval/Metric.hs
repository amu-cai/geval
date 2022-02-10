{-# LANGUAGE OverloadedStrings #-}

module GEval.Metric
  (Metric(..),
   MetricOrdering(..),
   defaultLogLossHashedSize,
   getMetricOrdering,
   bestPossibleValue,
   perfectOutLineFromExpectedLine,
   fixedNumberOfColumnsInExpected,
   fixedNumberOfColumnsInInput,
   metricCompare)
  where

import Data.Word
import Data.Text hiding (map)
import Data.Monoid ((<>))
import Control.Exception

import GEval.Common
import GEval.Clippings
import GEval.MatchingSpecification
import Data.Attoparsec.Text (parseOnly)

-- here metrics and their basic properties are listed,
-- the evaluation procedures are defined in GEval.Core

-- | evaluation metric
data Metric = RMSE | MSE | Pearson | Spearman | BLEU | GLEU | WER | CER | Accuracy MatchingSpecification | ClippEU
              | FMeasure Double | MacroFMeasure Double | NMI
              | LogLossHashed Word32 | CharMatch | MAP | LogLoss | Likelihood
              | BIOF1 | BIOWeightedF1 | BIOF1Labels | TokenAccuracy | SegmentAccuracy | LikelihoodHashed Word32 | PerplexityHashed Word32
              | MAE | SMAPE
              | MultiLabelFMeasure Double MatchingSpecification
              | MultiLabelLogLoss | MultiLabelLikelihood
              | SoftFMeasure Double
              | ProbabilisticMultiLabelFMeasure Double
              | ProbabilisticSoftFMeasure Double
              | ProbabilisticSoft2DFMeasure Double
              | Soft2DFMeasure Double
              | FLCFMeasure Double
              | Haversine
              | Improvement Double
              -- it would be better to avoid infinite recursion here
              -- `Mean (Mean BLEU)` is not useful, but as it would mean
              -- a larger refactor, we will postpone this
              | Mean Metric
              | MacroAvg Metric
              | MAEAgainstInterval | MSEAgainstInterval | RMSEAgainstInterval
              deriving (Eq)

instance Show Metric where
  show RMSE = "RMSE"
  show MSE  = "MSE"
  show Pearson = "Pearson"
  show Spearman = "Spearman"
  show BLEU = "BLEU"
  show GLEU = "GLEU"
  show WER = "WER"
  show CER = "CER"
  show (Accuracy ExactMatch) = "Accuracy"
  show (Accuracy FuzzyMatch) = "Fuzzy/" ++ (show $ Accuracy ExactMatch)
  show (Accuracy (CutLabel matchSpec)) = "CutLabel/" ++ (show $ Accuracy matchSpec)
  show (Accuracy (SmartMatch matchSpec)) = "Smart/" ++ (show $ Accuracy matchSpec)
  show (Accuracy (Harden matchSpec)) = "Harden/" ++ (show $ Accuracy matchSpec)
  show (Accuracy (LenientHarden matchSpec)) = "LenientHarden/" ++ (show $ Accuracy matchSpec)
  show (Accuracy (Lower matchSpec)) = "Lower/" ++ (show $ Accuracy matchSpec)
  show (Accuracy (ExtractNumber matchSpec)) = "ExtractNumber/" ++ (show $ Accuracy matchSpec)
  show ClippEU = "ClippEU"
  show (FMeasure beta) = "F" ++ (show beta)
  show (MacroFMeasure beta) = "Macro-F" ++ (show beta)
  show (SoftFMeasure beta) = "Soft-F" ++ (show beta)
  show (ProbabilisticMultiLabelFMeasure beta) = "Probabilistic-MultiLabel-F" ++ (show beta)
  show (ProbabilisticSoftFMeasure beta) = "Probabilistic-Soft-F" ++ (show beta)
  show (ProbabilisticSoft2DFMeasure beta) = "Probabilistic-Soft2D-F" ++ (show beta)
  show (Soft2DFMeasure beta) = "Soft2D-F" ++ (show beta)
  show (FLCFMeasure beta) = "FLC-F" ++ (show beta)
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
  show (PerplexityHashed nbOfBits) = "PerplexityHashed" ++ (if
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
  show BIOWeightedF1 = "BIO-Weighted-F1"
  show TokenAccuracy = "TokenAccuracy"
  show SegmentAccuracy = "SegmentAccuracy"
  show MAE = "MAE"
  show SMAPE = "SMAPE"
  show (MultiLabelFMeasure beta ExactMatch) = "MultiLabel-F" ++ (show beta)
  show (MultiLabelFMeasure beta FuzzyMatch) = "Fuzzy/" ++ (show $ MultiLabelFMeasure beta ExactMatch)
  show (MultiLabelFMeasure beta (CutLabel matchSpec)) = "CutLabel/" ++ (show $ MultiLabelFMeasure beta matchSpec)
  show (MultiLabelFMeasure beta (SmartMatch matchSpec)) = "Smart/" ++ (show $ MultiLabelFMeasure beta matchSpec)
  show (MultiLabelFMeasure beta (Harden matchSpec)) = "Harden/" ++ (show $ MultiLabelFMeasure beta matchSpec)
  show (MultiLabelFMeasure beta (LenientHarden matchSpec)) = "LenientHarden/" ++ (show $ MultiLabelFMeasure beta matchSpec)
  show (MultiLabelFMeasure beta (Lower matchSpec)) = "Lower/" ++ (show $ MultiLabelFMeasure beta matchSpec)
  show (MultiLabelFMeasure beta (ExtractNumber matchSpec)) = "ExtractNumber/" ++ (show $ MultiLabelFMeasure beta matchSpec)
  show MultiLabelLogLoss = "MultiLabel-Logloss"
  show MultiLabelLikelihood = "MultiLabel-Likelihood"
  show Haversine = "Haversine"
  show (Improvement threshold) = "Improvement@" ++ (show threshold)
  show (Mean metric) = "Mean/" ++ (show metric)
  show (MacroAvg metric) = "MacroAvg/" ++ (show metric)
  show MAEAgainstInterval = "MAE-Against-Interval"
  show MSEAgainstInterval = "MSE-Against-Interval"
  show RMSEAgainstInterval = "RMSE-Against-Interval"

applyMatchingSpecification :: (MatchingSpecification -> MatchingSpecification)
                           -> Metric
                           -> Metric
applyMatchingSpecification fun (MultiLabelFMeasure beta matchSpec)
  = MultiLabelFMeasure beta (fun matchSpec)
applyMatchingSpecification fun (Accuracy matchSpec)
  = Accuracy (fun matchSpec)
applyMatchingSpecification _ metric = error $ "Matching specification cannot be applied to the " ++ (show metric) ++ " metric"

instance Read Metric where
  readsPrec p ('M':'e':'a':'n':'/':theRest) = case readsPrec p theRest of
    [(metric, theRest')] -> [(Mean metric, theRest')]
    _ -> []
  readsPrec p ('M':'a':'c':'r':'o':'A':'v':'g':'/':theRest) = case readsPrec p theRest of
    [(metric, theRest')] -> [(MacroAvg metric, theRest')]
    _ -> []
  readsPrec p ('F':'u':'z':'z':'y':'/':theRest) = case readsPrec p theRest of
    [(metric, theRest')] -> [(applyMatchingSpecification (const FuzzyMatch) metric, theRest')]
    _ -> []
  readsPrec p ('C':'u':'t':'L':'a':'b':'e':'l':'/':theRest) = case readsPrec p theRest of
    [(metric, theRest')] -> [(applyMatchingSpecification CutLabel metric, theRest')]
    _ -> []
  readsPrec p ('S':'m':'a':'r':'t':'/':theRest) = case readsPrec p theRest of
    [(metric, theRest')] -> [(applyMatchingSpecification SmartMatch metric, theRest')]
    _ -> []
  readsPrec p ('H':'a':'r':'d':'e':'n':'/':theRest) = case readsPrec p theRest of
    [(metric, theRest')] -> [(applyMatchingSpecification Harden metric, theRest')]
    _ -> []
  readsPrec p ('L':'e':'n':'i':'e':'n':'t':'H':'a':'r':'d':'e':'n':'/':theRest) = case readsPrec p theRest of
    [(metric, theRest')] -> [(applyMatchingSpecification LenientHarden metric, theRest')]
    _ -> []
  readsPrec p ('L':'o':'w':'e':'r':'/':theRest) = case readsPrec p theRest of
    [(metric, theRest')] -> [(applyMatchingSpecification Lower metric, theRest')]
    _ -> []
  readsPrec p ('E':'x':'t':'r':'a':'c':'t':'N':'u':'m':'b':'e':'r':'/':theRest) = case readsPrec p theRest of
    [(metric, theRest')] -> [(applyMatchingSpecification ExtractNumber metric, theRest')]
    _ -> []
  readsPrec _ ('R':'M':'S':'E':'-':'A':'g':'a':'i':'n':'s':'t':'-':'I':'n':'t':'e':'r':'v':'a':'l':theRest) = [(RMSEAgainstInterval, theRest)]
  readsPrec _ ('R':'M':'S':'E':theRest) = [(RMSE, theRest)]
  readsPrec _ ('M':'S':'E':'-':'A':'g':'a':'i':'n':'s':'t':'-':'I':'n':'t':'e':'r':'v':'a':'l':theRest) = [(MSEAgainstInterval, theRest)]
  readsPrec _ ('M':'S':'E':theRest) = [(MSE, theRest)]
  readsPrec _ ('P':'e':'a':'r':'s':'o':'n':theRest) = [(Pearson, theRest)]
  readsPrec _ ('S':'p':'e':'a':'r':'m':'a':'n':theRest) = [(Spearman, theRest)]
  readsPrec _ ('B':'L':'E':'U':theRest) = [(BLEU, theRest)]
  readsPrec _ ('G':'L':'E':'U':theRest) = [(GLEU, theRest)]
  readsPrec _ ('W':'E':'R':theRest) = [(WER, theRest)]
  readsPrec _ ('C':'E':'R':theRest) = [(CER, theRest)]
  readsPrec _ ('A':'c':'c':'u':'r':'a':'c':'y':theRest) = [(Accuracy ExactMatch, theRest)]
  readsPrec _ ('C':'l':'i':'p':'p':'E':'U':theRest) = [(ClippEU, theRest)]
  readsPrec _ ('N':'M':'I':theRest) = [(NMI, theRest)]
  readsPrec p ('F':'L':'C':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest')] -> [(FLCFMeasure beta, theRest')]
    _ -> []
  readsPrec p ('F':theRest) = case readsPrec p theRest of
    [(beta, theRest')] -> [(FMeasure beta, theRest')]
    _ -> []
  readsPrec p ('M':'a':'c':'r':'o':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest')] -> [(MacroFMeasure beta, theRest')]
    _ -> []
  readsPrec p ('M':'u':'l':'t':'i':'L':'a':'b':'e':'l':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest')] -> [(MultiLabelFMeasure beta ExactMatch, theRest')]
    _ -> []
  readsPrec p ('S':'o':'f':'t':'2':'D':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest')] -> [(Soft2DFMeasure beta, theRest')]
    _ -> []
  readsPrec p ('S':'o':'f':'t':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest')] -> [(SoftFMeasure beta, theRest')]
    _ -> []
  readsPrec p ('P':'r':'o':'b':'a':'b':'i':'l':'i':'s':'t':'i':'c':'-':'M':'u':'l':'t':'i':'L':'a':'b':'e':'l':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest')] -> [(ProbabilisticMultiLabelFMeasure beta, theRest')]
    _ -> []
  readsPrec p ('P':'r':'o':'b':'a':'b':'i':'l':'i':'s':'t':'i':'c':'-':'S':'o':'f':'t':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest')] -> [(ProbabilisticSoftFMeasure beta, theRest')]
    _ -> []
  readsPrec p ('P':'r':'o':'b':'a':'b':'i':'l':'i':'s':'t':'i':'c':'-':'S':'o':'f':'t':'2':'D':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest')] -> [(ProbabilisticSoft2DFMeasure beta, theRest')]
    _ -> []
  readsPrec p ('L':'o':'g':'L':'o':'s':'s':'H':'a':'s':'h':'e':'d':theRest) = case readsPrec p theRest of
    [(nbOfBits, theRest')] -> [(LogLossHashed nbOfBits, theRest')]
    _ -> [(LogLossHashed defaultLogLossHashedSize, theRest)]
  readsPrec p ('L':'i':'k':'e':'l':'i':'h':'o':'o':'d':'H':'a':'s':'h':'e':'d':theRest) = case readsPrec p theRest of
    [(nbOfBits, theRest')] -> [(LikelihoodHashed nbOfBits, theRest')]
    _ -> [(LikelihoodHashed defaultLogLossHashedSize, theRest)]
  readsPrec p ('P':'e':'r':'p':'l':'e':'x':'i':'t':'y':'H':'a':'s':'h':'e':'d':theRest) = case readsPrec p theRest of
    [(nbOfBits, theRest')] -> [(PerplexityHashed nbOfBits, theRest')]
    _ -> [(PerplexityHashed defaultLogLossHashedSize, theRest)]
  readsPrec _ ('L':'o':'g':'L':'o':'s':'s':theRest) = [(LogLoss, theRest)]
  readsPrec _ ('L':'i':'k':'e':'l':'i':'h':'o':'o':'d':theRest) = [(Likelihood, theRest)]
  readsPrec _ ('C':'h':'a':'r':'M':'a':'t':'c':'h':theRest) = [(CharMatch, theRest)]
  readsPrec _ ('M':'A':'P':theRest) = [(MAP, theRest)]
  readsPrec _ ('B':'I':'O':'-':'F':'1':'-':'L':'a':'b':'e':'l':'s':theRest) = [(BIOF1Labels, theRest)]
  readsPrec _ ('B':'I':'O':'-':'W':'e':'i':'g':'h':'t':'e':'d':'-':'F':'1': theRest) = [(BIOWeightedF1, theRest)]
  readsPrec _ ('B':'I':'O':'-':'F':'1':theRest) = [(BIOF1, theRest)]
  readsPrec _ ('T':'o':'k':'e':'n':'A':'c':'c':'u':'r':'a':'c':'y':theRest) = [(TokenAccuracy, theRest)]
  readsPrec _ ('S':'e':'g':'m':'e':'n':'t':'A':'c':'c':'u':'r':'a':'c':'y':theRest) = [(SegmentAccuracy, theRest)]
  readsPrec _ ('M':'A':'E':'-':'A':'g':'a':'i':'n':'s':'t':'-':'I':'n':'t':'e':'r':'v':'a':'l':theRest) = [(MAEAgainstInterval, theRest)]
  readsPrec _ ('M':'A':'E':theRest) = [(MAE, theRest)]
  readsPrec _ ('S':'M':'A':'P':'E':theRest) = [(SMAPE, theRest)]
  readsPrec _ ('M':'u':'l':'t':'i':'L':'a':'b':'e':'l':'-':'L':'o':'g':'L':'o':'s':'s':theRest) = [(MultiLabelLogLoss, theRest)]
  readsPrec _ ('M':'u':'l':'t':'i':'L':'a':'b':'e':'l':'-':'L':'i':'k':'e':'l':'i':'h':'o':'o':'d':theRest) = [(MultiLabelLikelihood, theRest)]
  readsPrec _ ('H':'a':'v':'e':'r':'s':'i':'n':'e':theRest) = [(Haversine, theRest)]
  readsPrec p ('I':'m':'p':'r':'o':'v':'e':'m':'e':'n':'t':'@':theRest) = case readsPrec p theRest of
    [(threshold, theRest')] -> [(Improvement threshold, theRest')]
    _ -> []
  readsPrec _ t = throw $ UnknownMetric t


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
getMetricOrdering CER      = TheLowerTheBetter
getMetricOrdering (Accuracy _) = TheHigherTheBetter
getMetricOrdering ClippEU  = TheHigherTheBetter
getMetricOrdering (FMeasure _) = TheHigherTheBetter
getMetricOrdering (MacroFMeasure _) = TheHigherTheBetter
getMetricOrdering (SoftFMeasure _) = TheHigherTheBetter
getMetricOrdering (ProbabilisticMultiLabelFMeasure _) = TheHigherTheBetter
getMetricOrdering (ProbabilisticSoftFMeasure _) = TheHigherTheBetter
getMetricOrdering (ProbabilisticSoft2DFMeasure _) = TheHigherTheBetter
getMetricOrdering (Soft2DFMeasure _) = TheHigherTheBetter
getMetricOrdering (FLCFMeasure _) = TheHigherTheBetter
getMetricOrdering NMI = TheHigherTheBetter
getMetricOrdering (LogLossHashed _) = TheLowerTheBetter
getMetricOrdering (LikelihoodHashed _) = TheHigherTheBetter
getMetricOrdering (PerplexityHashed _) = TheLowerTheBetter
getMetricOrdering CharMatch = TheHigherTheBetter
getMetricOrdering MAP = TheHigherTheBetter
getMetricOrdering LogLoss = TheLowerTheBetter
getMetricOrdering Likelihood = TheHigherTheBetter
getMetricOrdering BIOF1 = TheHigherTheBetter
getMetricOrdering BIOWeightedF1 = TheHigherTheBetter
getMetricOrdering BIOF1Labels = TheHigherTheBetter
getMetricOrdering TokenAccuracy = TheHigherTheBetter
getMetricOrdering SegmentAccuracy = TheHigherTheBetter
getMetricOrdering MAE = TheLowerTheBetter
getMetricOrdering SMAPE = TheLowerTheBetter
getMetricOrdering (MultiLabelFMeasure _ _) = TheHigherTheBetter
getMetricOrdering MultiLabelLogLoss = TheLowerTheBetter
getMetricOrdering MultiLabelLikelihood = TheHigherTheBetter
getMetricOrdering Haversine = TheLowerTheBetter
getMetricOrdering (Improvement _) = TheHigherTheBetter
getMetricOrdering MSEAgainstInterval = TheLowerTheBetter
getMetricOrdering RMSEAgainstInterval = TheLowerTheBetter
getMetricOrdering MAEAgainstInterval = TheLowerTheBetter
getMetricOrdering (Mean metric) = getMetricOrdering metric
getMetricOrdering (MacroAvg metric) = getMetricOrdering metric

metricCompare :: Metric -> MetricValue -> MetricValue -> Ordering
metricCompare metric a b = metricCompare' (getMetricOrdering metric) a b
  where metricCompare' TheHigherTheBetter a' b' = a' `compare` b'
        metricCompare' TheLowerTheBetter a' b' = b' `compare` a'

bestPossibleValue :: Metric -> Maybe MetricValue
bestPossibleValue (Mean metric) = bestPossibleValue metric
bestPossibleValue (MacroAvg metric) = bestPossibleValue metric
bestPossibleValue (Improvement _) = Nothing
bestPossibleValue (PerplexityHashed _) = Just 1.0
bestPossibleValue metric = case getMetricOrdering metric of
  TheLowerTheBetter -> Just 0.0
  TheHigherTheBetter -> Just 1.0

fixedNumberOfColumnsInExpected :: Metric -> Bool
fixedNumberOfColumnsInExpected (Mean metric) = fixedNumberOfColumnsInExpected metric
fixedNumberOfColumnsInExpected (MacroAvg metric) = fixedNumberOfColumnsInExpected metric
fixedNumberOfColumnsInExpected MAP = False
fixedNumberOfColumnsInExpected BLEU = False
fixedNumberOfColumnsInExpected GLEU = False
fixedNumberOfColumnsInExpected (Accuracy ExactMatch) = True
fixedNumberOfColumnsInExpected (Accuracy _) = False
fixedNumberOfColumnsInExpected _ = True

fixedNumberOfColumnsInInput :: Metric -> Bool
fixedNumberOfColumnsInInput (Mean metric) = fixedNumberOfColumnsInInput metric
fixedNumberOfColumnsInInput (MacroAvg metric) = fixedNumberOfColumnsInInput metric
fixedNumberOfColumnsInInput (SoftFMeasure _) = False
fixedNumberOfColumnsInInput (ProbabilisticSoftFMeasure _) = False
fixedNumberOfColumnsInInput (Soft2DFMeasure _) = False
fixedNumberOfColumnsInInput _ = True


perfectOutLineFromExpectedLine :: Metric -> Text -> Text
perfectOutLineFromExpectedLine (Mean metric) t = perfectOutLineFromExpectedLine metric t
perfectOutLineFromExpectedLine (MacroAvg metric) t = perfectOutLineFromExpectedLine metric t
perfectOutLineFromExpectedLine (LogLossHashed _) t = addProbOne t
perfectOutLineFromExpectedLine (LikelihoodHashed _) t = addProbOne t
perfectOutLineFromExpectedLine (PerplexityHashed _) t = addProbOne t
perfectOutLineFromExpectedLine BLEU t = getFirstColumn t
perfectOutLineFromExpectedLine GLEU t = getFirstColumn t
perfectOutLineFromExpectedLine ClippEU t = cleanMarginFromClippEU t
perfectOutLineFromExpectedLine (Accuracy ExactMatch) t = t
perfectOutLineFromExpectedLine (Accuracy _) t = getFirstColumn t
perfectOutLineFromExpectedLine _ t = t

addProbOne :: Text -> Text
addProbOne = (<> ":1.0")

getFirstColumn :: Text -> Text
getFirstColumn t = case splitOn "\t" t of
  [] -> ""
  (h:_) -> h

cleanMarginFromClippEU :: Text -> Text
cleanMarginFromClippEU t = Data.Text.unwords outs
  where outs = map toOut specs
        (Right specs) = parseOnly lineClippingSpecsParser t
        toOut (ClippingSpec page rect _) =
          pack $ show (Clipping page rect)

defaultLogLossHashedSize :: Word32
defaultLogLossHashedSize = 10
