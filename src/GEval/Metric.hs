{-# LANGUAGE OverloadedStrings #-}

module GEval.Metric
  (Metric(..),
   MetricOrdering(..),
   defaultLogLossHashedSize,
   getMetricOrdering,
   bestPossibleValue,
   perfectOutLineFromExpectedLine,
   fixedNumberOfColumnsInExpected,
   fixedNumberOfColumnsInInput)
  where

import Data.Word
import Data.Text
import Data.Monoid ((<>))

import GEval.Common
import GEval.ClippEU
import Data.Attoparsec.Text (parseOnly)

-- here metrics and their basic properties are listed,
-- the evaluation procedures are defined in GEval.Core

-- | evaluation metric
data Metric = RMSE | MSE | Pearson | Spearman | BLEU | GLEU | WER | Accuracy | ClippEU
              | FMeasure Double | MacroFMeasure Double | NMI
              | LogLossHashed Word32 | CharMatch | MAP | LogLoss | Likelihood
              | BIOF1 | BIOF1Labels | TokenAccuracy | LikelihoodHashed Word32 | MAE | SMAPE | MultiLabelFMeasure Double
              | MultiLabelLogLoss | MultiLabelLikelihood
              | SoftFMeasure Double | ProbabilisticSoftFMeasure Double
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
  show (ProbabilisticSoftFMeasure beta) = "Probabilistic-Soft-F" ++ (show beta)
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
  show TokenAccuracy = "TokenAccuracy"
  show MAE = "MAE"
  show SMAPE = "SMAPE"
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
  readsPrec p ('P':'r':'o':'b':'a':'b':'i':'l':'i':'s':'t':'i':'c':'-':'S':'o':'f':'t':'-':'F':theRest) = case readsPrec p theRest of
    [(beta, theRest)] -> [(ProbabilisticSoftFMeasure beta, theRest)]
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
  readsPrec _ ('T':'o':'k':'e':'n':'A':'c':'c':'u':'r':'a':'c':'y':theRest) = [(TokenAccuracy, theRest)]
  readsPrec _ ('M':'A':'E':theRest) = [(MAE, theRest)]
  readsPrec _ ('S':'M':'A':'P':'E':theRest) = [(SMAPE, theRest)]
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
getMetricOrdering (ProbabilisticSoftFMeasure _) = TheHigherTheBetter
getMetricOrdering NMI = TheHigherTheBetter
getMetricOrdering (LogLossHashed _) = TheLowerTheBetter
getMetricOrdering (LikelihoodHashed _) = TheHigherTheBetter
getMetricOrdering CharMatch = TheHigherTheBetter
getMetricOrdering MAP = TheHigherTheBetter
getMetricOrdering LogLoss = TheLowerTheBetter
getMetricOrdering Likelihood = TheHigherTheBetter
getMetricOrdering BIOF1 = TheHigherTheBetter
getMetricOrdering BIOF1Labels = TheHigherTheBetter
getMetricOrdering TokenAccuracy = TheHigherTheBetter
getMetricOrdering MAE = TheLowerTheBetter
getMetricOrdering SMAPE = TheLowerTheBetter
getMetricOrdering (MultiLabelFMeasure _) = TheHigherTheBetter
getMetricOrdering MultiLabelLogLoss = TheLowerTheBetter
getMetricOrdering MultiLabelLikelihood = TheHigherTheBetter

bestPossibleValue :: Metric -> MetricValue
bestPossibleValue metric = case getMetricOrdering metric of
  TheLowerTheBetter -> 0.0
  TheHigherTheBetter -> 1.0

fixedNumberOfColumnsInExpected :: Metric -> Bool
fixedNumberOfColumnsInExpected MAP = False
fixedNumberOfColumnsInExpected BLEU = False
fixedNumberOfColumnsInExpected GLEU = False
fixedNumberOfColumnsInExpected _ = True

fixedNumberOfColumnsInInput :: Metric -> Bool
fixedNumberOfColumnsInInput (SoftFMeasure _) = False
fixedNumberOfColumnsInInput (ProbabilisticSoftFMeasure _) = False
fixedNumberOfColumnsInInput _ = True

perfectOutLineFromExpectedLine :: Metric -> Text -> Text
perfectOutLineFromExpectedLine (LogLossHashed _) t = t <> ":1.0"
perfectOutLineFromExpectedLine (LikelihoodHashed _) t = t <> ":1.0"
perfectOutLineFromExpectedLine BLEU t = getFirstColumn t
perfectOutLineFromExpectedLine GLEU t = getFirstColumn t
perfectOutLineFromExpectedLine ClippEU t = cleanMarginFromClippEU t
perfectOutLineFromExpectedLine _ t = t

getFirstColumn :: Text -> Text
getFirstColumn t = case splitOn "\t" t of
  [] -> ""
  (h:_) -> h

cleanMarginFromClippEU :: Text -> Text
cleanMarginFromClippEU t = Data.Text.unwords outs
  where outs = Prelude.map toOut specs
        (Right specs) = parseOnly lineClippingSpecsParser t
        toOut (ClippingSpec (PageNumber pageNumber) (Rectangle (Point x0 y0) (Point x1 y1)) _) =
          pack ((show pageNumber) ++ "/" ++ (show x0) ++ "," ++ (show y0) ++ "," ++ (show x1) ++ "," ++ (show y1))

defaultLogLossHashedSize :: Word32
defaultLogLossHashedSize = 10
