{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec

import GEval.Metric
import GEval.MetricsMeta (listOfAvailableEvaluationSchemes, isEvaluationSchemeDescribed, expectedScore, outContents)
import GEval.Core
import GEval.Common
import GEval.EvaluationScheme
import GEval.MatchingSpecification
import GEval.OptionsParser
import GEval.BLEU
import GEval.Clippings
import GEval.PrecisionRecall
import GEval.ClusteringMetrics
import GEval.BIO
import GEval.LineByLine
import GEval.ParseParams
import GEval.Submit
import Text.Tokenizer
import Text.WordShape
import Data.Attoparsec.Text
import Options.Applicative
import Data.Text
import Text.EditDistance
import GEval.Annotation
import GEval.BlackBoxDebugging
import GEval.FeatureExtractor
import GEval.Selector
import GEval.CreateChallenge
import GEval.Validation
import GEval.Confidence
import Data.Conduit.Bootstrap

import Data.Map.Strict
import Data.Conduit.List (consume)

import Data.NDCG (ndcgAt, binaryNdcgAt)

import System.FilePath

import System.Directory
import System.Process
import System.Exit
import System.IO
import System.IO.Temp
import System.IO.Silently

import Data.List (sort)

import qualified Test.HUnit as HU

import qualified Data.IntSet as IS
import qualified Data.Vector as V

import Data.Conduit.SmartSource
import Data.Conduit.Rank
import qualified Data.Conduit.Text as CT
import Data.Conduit
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC

import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (normalDistr)
import Data.Statistics.Kendall (kendall, kendallZ)
import qualified Data.Vector.Unboxed as DVU
--import qualified Statistics.Matrix.Types as SMT
import Data.Statistics.Loess (loess)
import Data.Statistics.Calibration (calibration)
import Data.CartesianStrings (parseCartesianString)
import Data.SplitIntoCrossTabs (splitIntoCrossTabs, CrossTab(..), TextFrag(..))

informationRetrievalBookExample :: [(String, Int)]
informationRetrievalBookExample = [("o", 2), ("o", 2), ("d", 2), ("x", 3), ("d", 3),
                                   ("x", 1), ("o", 1), ("x", 1), ( "x", 1), ("x", 1), ("x", 1),
                                   ("x", 2), ("o", 2), ("o", 2),
                                   ("x", 3), ("d", 3), ("d", 3)]

perfectClustering :: [(Int, Char)]
perfectClustering = [(0, 'a'), (2, 'b'), (3, 'c'), (2, 'b'), (2, 'b'), (1, 'd'), (0, 'a')]

stupidClusteringOneBigCluster :: [(Int, Int)]
stupidClusteringOneBigCluster = [(0, 2), (2, 2), (1, 2), (2, 2), (0, 2), (0, 2), (0, 2), (0, 2), (1, 2), (1, 2)]

stupidClusteringManySmallClusters :: [(Int, Int)]
stupidClusteringManySmallClusters = [(0, 0), (2, 1), (1, 2), (2, 3), (0, 4), (0, 5), (0, 6), (0, 7), (1, 8), (1, 9)]




main :: IO ()
main = hspec $ do
  describe "root mean square error" $ do
    it "simple test" $ do
      [(_, ((MetricOutput (SimpleRun val) _):_))] <-  geval $ defaultGEvalSpecification {gesExpectedDirectory=Just "test/rmse-simple/rmse-simple", gesOutDirectory="test/rmse-simple/rmse-simple-solution"}
      val `shouldBeAlmost` 0.64549722436790
  describe "mean square error" $ do
    it "simple test with arguments" $
      runGEvalTest "mse-simple" `shouldReturnAlmost` 0.4166666666666667
  describe "mean absolute error" $ do
    it "simple test with arguments" $
      runGEvalTest "mae-simple" `shouldReturnAlmost` 1.5
  describe "SMAPE" $ do
    it "simple test" $
      runGEvalTest "smape-simple" `shouldReturnAlmost` 45.1851851851852
  describe "Spearman's rank correlation coefficient" $ do
    it "simple test" $ do
      runGEvalTest "spearman-simple" `shouldReturnAlmost` (- 0.5735)
  describe "BLEU" $ do
    it "trivial example from Wikipedia" $
      runGEvalTest "bleu-trivial" `shouldReturnAlmost` 0.0
    it "complex example" $
      runGEvalTest "bleu-complex" `shouldReturnAlmost` 0.6211
    it "perfect translation" $
      runGEvalTest "bleu-perfect" `shouldReturnAlmost` 1.0000
    it "empty translation" $
      runGEvalTest "bleu-empty" `shouldReturnAlmost` 0.0000
    it "with tokenization" $
      runGEvalTest "bleu-with-tokenization" `shouldReturnAlmost` 0.6501914150070065
    it "with bootstrap" $
      runGEvalTest "bleu-complex-bootstrap" `shouldReturnAlmost` 0.6079109363640034
  describe "GLEU" $ do
    it "simple example" $
      runGEvalTest "gleu-simple" `shouldReturnAlmost` 0.462962962962963
    it "empty translation" $
      runGEvalTest "gleu-empty" `shouldReturnAlmost` 0.0
    it "perfect translation" $
      runGEvalTest "gleu-perfect" `shouldReturnAlmost` 1.0
  describe "WER" $ do
    it "simple example" $
      runGEvalTest "wer-simple" `shouldReturnAlmost` 0.5555555555
  describe "CER" $ do
    it "simple example" $
      runGEvalTest "cer-simple" `shouldReturnAlmost` 0.28947368421
    it "simple example (Mean/CER)" $
      runGEvalTest "cer-mean-simple" `shouldReturnAlmost` 0.277777777777778
    it "space escaping" $
      runGEvalTest "cer-space-escaping" `shouldReturnAlmost` 0.0555555
    it "with empty strings (corner cases)" $
      runGEvalTest "cer-empty-texts" `shouldReturnAlmost` 0.583325
  describe "Haversine" $ do
    it "simple example" $
      runGEvalTest "haversine" `shouldReturnAlmost` 1951.9351057250876
  describe "Improvement" $ do
    it "simple improvement" $ do
      runGEvalTest "improvement-simple" `shouldReturnAlmost` 1.12
  describe "Accuracy" $ do
    it "simple example" $
      runGEvalTest "accuracy-simple" `shouldReturnAlmost` 0.6
    it "with probs" $
      runGEvalTest "accuracy-probs" `shouldReturnAlmost` 0.4
    it "sorted" $
      runGEvalTest "accuracy-on-sorted" `shouldReturnAlmost` 0.75
    it "with filtering" $
      runGEvalTest "accuracy-filtering" `shouldReturnAlmost` 0.6666
    it "with filtering 2" $
      runGEvalTest "accuracy-multiple-filtering" `shouldReturnAlmost` 0.5
    it "with fuzzy match" $
      runGEvalTest "fuzzy-match-accuracy" `shouldReturnAlmost` 0.6
  describe "F-measure" $ do
    it "simple example" $
      runGEvalTest "f-measure-simple" `shouldReturnAlmost` 0.57142857
    it "perfect classifier" $
      runGEvalTest "f-measure-perfect" `shouldReturnAlmost` 1.0
    it "stupid classifier" $
      runGEvalTest "f-measure-stupid" `shouldReturnAlmost` 0.0
    it "all false" $
      runGEvalTest "f-measure-all-false" `shouldReturnAlmost` 1.0
    it "F2-measure" $
      runGEvalTest "f2-simple" `shouldReturnAlmost` 0.714285714
  describe "Macro-F-measure" $ do
    it "simple example" $
      runGEvalTest "macro-f1-simple" `shouldReturnAlmost` 0.266666
    it "perfect soltion" $
      runGEvalTest "macro-f-measure-perfect" `shouldReturnAlmost` 1.00000
  describe "TokenAccuracy" $ do
    it "simple example" $ do
       runGEvalTest "token-accuracy-simple" `shouldReturnAlmost` 0.5
  describe "SegmentAccuracy" $ do
    it "simple test" $ do
      runGEvalTest "segment-accuracy-simple" `shouldReturnAlmost` 0.4444444
  describe "precision count" $ do
    it "simple test" $ do
      precisionCount [["Alice", "has", "a", "cat" ]] ["Ala", "has", "cat"] `shouldBe` 2
    it "none found" $ do
      precisionCount [["Alice", "has", "a", "cat" ]] ["for", "bar", "baz"] `shouldBe` 0
    it "multiple values" $ do
      precisionCount [["bar", "bar", "bar", "bar", "foo", "xyz", "foo"]] ["foo", "bar", "foo", "baz", "bar", "foo"] `shouldBe` 4
    it "multiple refs" $ do
      precisionCount [["foo", "baz"], ["bar"], ["baz", "xyz"]]  ["foo", "bar", "foo"] `shouldBe` 2
  describe "purity (in flat clustering)" $ do
    it "the example from Information Retrieval Book" $ do
      purity informationRetrievalBookExample `shouldBeAlmost` 0.70588
  describe "NMI (in flat clustering)" $ do
    it "the example from Information Retrieval Book" $ do
      normalizedMutualInformation informationRetrievalBookExample `shouldBeAlmost` 0.36456
    it "perfect clustering" $ do
      normalizedMutualInformation perfectClustering `shouldBeAlmost` 1.0
    it "stupid clustering with one big cluster" $ do
      normalizedMutualInformation stupidClusteringOneBigCluster `shouldBeAlmost` 0.0
    it "stupid clustering with many small clusters" $ do
      normalizedMutualInformation stupidClusteringManySmallClusters `shouldBeAlmost` 0.61799
  describe "NMI challenge" $ do
    it "complex test" $ do
      runGEvalTest "nmi-complex" `shouldReturnAlmost` 0.36456
  describe "LogLossHashed challenge" $ do
    it "simple example" $ do
      runGEvalTest "log-loss-hashed-simple" `shouldReturnAlmost` 2.398479083333333
    it "example with unnormalized values" $ do
      runGEvalTest "log-loss-hashed-not-normalized" `shouldReturnAlmost` 1.0468455186722887
    it "with probs instead of log probs" $ do
      runGEvalTest "log-loss-hashed-probs" `shouldReturnAlmost` 4.11631293099392
    it "with probs instead of log probs (with normalization)" $ do
      runGEvalTest "log-loss-hashed-probs-normalized" `shouldReturnAlmost` 1.55537749098853
    it "with log probs whose probs are summing up to less than 1.0" $ do
      runGEvalTest "log-loss-hashed-normalization" `shouldReturnAlmost` 5.16395069238851
  describe "PerplexityHashed challenge" $ do
    it "simple example" $ do
      runGEvalTest "perplexity-hashed-simple" `shouldReturnAlmost` 11.006423790840
  describe "LikelihoodHashed challenge" $ do
    it "example with unnormalized values" $ do
      runGEvalTest "likelihood-hashed-not-normalized" `shouldReturnAlmost` 0.351043364110715

  describe "reading options" $ do
    it "can get the metric" $ do
      extractMetric "bleu-complex" `shouldReturn` (Just BLEU)
  describe "error handling" $ do
    it "too few lines are handled" $ do
      runGEvalTest "error-too-few-lines" `shouldThrow` (== TooFewLines)
    it "too many lines are handled" $ do
      runGEvalTest "error-too-many-lines" `shouldThrow` (== TooManyLines)
    it "empty output is handled" $ do
      runGEvalTest "empty-output" `shouldThrow` (== EmptyOutput)
    it "unexpected data is handled" $
      runGEvalTest "unexpected-data" `shouldThrow` (== UnexpectedData 3 "input does not start with a digit")
    it "unwanted data is handled" $
      runGEvalTest "unwanted-data" `shouldThrow` (== UnexpectedData 2 "number expected")
  describe "custom metric" $ do
    it "weighted accuracy" $ do
      runGEvalTest "weighted-accuracy" `shouldReturnAlmost` 0.65
  describe "precision and recall" $ do
    it "null test" $ do
      precision neverMatch ['a', 'b', 'c'] [0, 1, 2, 3, 4, 5] `shouldBeAlmost` 0.0
      recall neverMatch ['a', 'b', 'c'] [0, 1, 2, 3, 4, 5] `shouldBeAlmost` 0.0
      f1Measure neverMatch ['a', 'b', 'c'] [0, 1, 2, 3, 4, 5] `shouldBeAlmost` 0.0
    it "basic test" $ do
      precision testMatchFun ['a', 'b', 'c'] [0, 1, 2, 3, 4, 5] `shouldBeAlmost` 0.3333333333333333
      recall testMatchFun ['a', 'b', 'c'] [0, 1, 2, 3, 4, 5] `shouldBeAlmost` 0.66666666666666666
      f1Measure testMatchFun ['a', 'b', 'c'] [0, 1, 2, 3, 4, 5] `shouldBeAlmost` 0.444444444444444
    it "perfect result" $ do
      precision alwaysMatch ['a', 'b', 'c'] [0, 1, 2] `shouldBeAlmost` 1.0
      recall alwaysMatch ['a', 'b', 'c'] [0, 1, 2] `shouldBeAlmost` 1.0
      f1Measure alwaysMatch ['a', 'b', 'c'] [0, 1, 2] `shouldBeAlmost` 1.0
    it "full match" $ do
      precision alwaysMatch ['a', 'b', 'c'] [0, 1, 2, 3, 4, 5] `shouldBeAlmost` 0.5
      recall alwaysMatch ['a', 'b', 'c'] [0, 1, 2, 3, 4, 5] `shouldBeAlmost` 1.0
      f1Measure alwaysMatch ['a', 'b', 'c'] [0, 1, 2, 3 , 4, 5] `shouldBeAlmost` 0.66666666666666
  describe "max match" $ do
    it "simple" $ do
      maxMatch (==) [1,2,2] [3,2] `shouldBe` 1
      maxMatch (==) [3,2] [1,2,2] `shouldBe` 1
  describe "ClippEU" $ do
    it "parsing rectangles" $ do
      let (Right r) = parseOnly (lineClippingsParser <* endOfInput) "2/0,0,2,3 10/20,30,40,50 18/0,1,500,3 "
      r `shouldBe` [Clipping (Just (PageNumber 2)) (Rectangle (Point 0 0) (Point 2 3)),
                    Clipping (Just (PageNumber 10)) (Rectangle (Point 20 30) (Point 40 50)),
                    Clipping (Just (PageNumber 18)) (Rectangle (Point 0 1) (Point 500 3))]
    it "parsing labeled rectangles" $ do
      let (Right r) = parseOnly (lineLabeledClippingsParser <* endOfInput) "2/0,0,2,3 foo:5/10,10,20,20 "
      r `shouldBe` [LabeledClipping Nothing $ Clipping (Just (PageNumber 2)) (Rectangle (Point 0 0) (Point 2 3)),
                    LabeledClipping (Just "foo") $ Clipping (Just (PageNumber 5)) (Rectangle (Point 10 10) (Point 20 20))]
    it "check partition" $ do
      partitionClippings (LabeledClipping Nothing (Clipping (Just (PageNumber 5)) $ Rectangle (Point 0 0) (Point 100 50)))
                         (LabeledClipping Nothing (Clipping (Just (PageNumber 5)) $ Rectangle (Point 10 20) (Point 200 300)))
        `shouldBe` Just (Rectangle (Point 10 20) (Point 100 50),
                         [LabeledClipping Nothing (Clipping (Just (PageNumber 5)) $ Rectangle (Point 10 0) (Point 100 19)),
                          LabeledClipping Nothing (Clipping (Just (PageNumber 5)) $ Rectangle (Point 0 0) (Point 9 50))],
                         [LabeledClipping Nothing (Clipping (Just (PageNumber 5)) $ Rectangle (Point 10 51) (Point 100 300)),
                          LabeledClipping Nothing (Clipping (Just (PageNumber 5)) $ Rectangle (Point 101 20) (Point 200 300))])
      partitionClippings (LabeledClipping (Just "bar") (Clipping (Just (PageNumber 10)) (Rectangle (Point 100 100) (Point 200 149))))
                         (LabeledClipping (Just "bar") (Clipping (Just (PageNumber 10)) (Rectangle (Point 100 201) (Point 200 300))))
        `shouldBe` Nothing
    it "no rectangles" $ do
      let (Right r) = parseOnly (lineClippingsParser <* endOfInput) ""
      r `shouldBe` []
    it "just spaces" $ do
      let (Right r) = parseOnly lineClippingsParser "     "
      r `shouldBe` []
    it "parsing specs" $ do
      let (Right r) = parseOnly lineClippingSpecsParser  " 2/0,0,2,3/5  10/20,30,40,50/10"
      r `shouldBe` [ClippingSpec (Just (PageNumber 2)) (Rectangle (Point 5 5) (Point 0 0))
                                                       (Rectangle (Point 0 0) (Point 7 8)),
                    ClippingSpec (Just (PageNumber 10)) (Rectangle (Point 30 40) (Point 30 40))
                                                        (Rectangle (Point 10 20) (Point 50 60))]
    it "full test" $ do
      runGEvalTest "clippeu-simple" `shouldReturnAlmost` 0.399999999999
  describe "evaluation metric specification is parsed" $ do
    it "for simple names" $ do
      let metrics = [RMSE, MSE, BLEU, Accuracy ExactMatch, ClippEU]
      let parsedMetrics = Prelude.map (read . show) metrics
      metrics `shouldBe` parsedMetrics
    it "for F-Measure" $ do
      read "F2" `shouldBe` (FMeasure 2.0)
      read "F1" `shouldBe` (FMeasure 1.0)
      read "F0.5" `shouldBe` (FMeasure 0.5)
  describe "Probabilistic-F1" $ do
    it "simple test" $ do
      runGEvalTest "probabilistic-f1-simple" `shouldReturnAlmost` 0.5
    it "with probs" $ do
      runGEvalTest "probabilistic-f1-probs" `shouldReturnAlmost` 0.5451223333805993
  describe "Soft-F1" $ do
    it "simple test" $ do
      runGEvalTest "soft-f1-simple" `shouldReturnAlmost` 0.33333333333333
    it "perfect test" $ do
      runGEvalTest "soft-f1-perfect" `shouldReturnAlmost` 1.0
  describe "FLC-F1" $ do
    it "simple test" $ do
      runGEvalTest "flc-f1-simple" `shouldReturnAlmost` 0.394231
    it "test with multi overlap" $ do
      runGEvalTest "flc-f1-multi-overlap" `shouldReturnAlmost` 0.588364
  describe "Probabilistic-Soft-F1" $ do
    it "simple test" $ do
      runGEvalTest "probabilistic-soft-f1-simple" `shouldReturnAlmost` 0.33333333333333
    it "simple test with perfect calibration" $ do
      runGEvalTest "probabilistic-soft-f1-calibrated" `shouldReturnAlmost` 0.88888888888
  describe "Soft2D-F1" $ do
    it "simple test" $ do
      runGEvalTest "soft2d-f1-simple" `shouldReturnAlmost` 0.22053934201995676
    it "very narrow rectangles" $ do
      runGEvalTest "soft2d-f1-one-pixel" `shouldReturnAlmost` 0.281992045358382
  describe "test edit-distance library" $ do
    it "for handling UTF8" $ do
      levenshteinDistance defaultEditCosts "źdźbło" "źd好bło" `shouldBe` 1
      levenshteinDistance defaultEditCosts "źdźbło" "źdźcło" `shouldBe` 1
  describe "CharMatch" $ do
    it "simple test" $ do
      runGEvalTest "charmatch-simple" `shouldReturnAlmost` 0.3571428571428571
    it "perfect solution" $ do
      runGEvalTest "charmatch-perfect" `shouldReturnAlmost` 1.0
    it "more complex test" $ do
      runGEvalTest "charmatch-complex" `shouldReturnAlmost` 0.1923076923076923
    it "broken test without input" $ do
      runGEvalTest "charmatch-no-input" `shouldThrow` (== NoInputFile "test/charmatch-no-input/charmatch-no-input/test-A/in.tsv")
  describe "MAP" $ do
    it "one result" $ do
      (calculateMAPForOneResult ["Berlin", "London", "Warsaw"]
                                ["Warsaw", "Moscow", "Berlin", "Prague"]) `shouldBeAlmost` 0.55555555
    it "check whether you cannot cheat with duplicated results" $ do
      (calculateMAPForOneResult ["one", "two"]
                                ["one", "one"]) `shouldBeAlmost` 0.5
    it "simple test" $ do
      runGEvalTest "map-simple" `shouldReturnAlmost` 0.444444444
    it "dos-end-of-lines" $
      runGEvalTest "dos-end-of-line" `shouldReturnAlmost` 0.75
  describe "NDCG" $ do
    it "simple test" $ do
      runGEvalTest "ndcg-simple" `shouldReturnAlmost` 0.71193675
  describe "LogLoss" $ do
    it "simple" $ do
      runGEvalTest "logloss-simple" `shouldReturnAlmost` 0.31824
    it "perfect" $ do
      runGEvalTest "logloss-perfect" `shouldReturnAlmost` 0.0
  describe "Likelihood" $ do
    it "simple" $ do
      runGEvalTest "likelihood-simple" `shouldReturnAlmost` 0.72742818469866
  describe "MultiLabel-F" $ do
    it "simple" $ do
      runGEvalTest "multilabel-f1-simple" `shouldReturnAlmost` 0.66666666666
    it "simple F2" $ do
      runGEvalTest "multilabel-f2-simple" `shouldReturnAlmost` 0.441176470588235
    it "labels given with probs" $ do
      runGEvalTest "multilabel-f1-with-probs" `shouldReturnAlmost` 0.615384615384615
    it "labels given with probs and numbers" $ do
      runGEvalTest "multilabel-f1-with-probs-and-numbers" `shouldReturnAlmost` 0.6666666666666
    it "information extraction" $ do
      runGEvalTest "multilabel-f1-ie" `shouldReturnAlmost` 0.1111111111
    it "information extraction with flags" $ do
      runGEvalTest "multilabel-f1-ie-flags" `shouldReturnAlmost` 0.444444444444
    it "information extraction with fuzzy matching" $ do
      runGEvalTest "multilabel-f1-ie-fuzzy" `shouldReturnAlmost` 0.681777777777
    it "information extraction with smart fuzzy matching" $ do
      runGEvalTest "multilabel-f1-ie-fuzzy-smart" `shouldReturnAlmost` 0.598444
    it "information extraction with smart fuzzy matching hardened" $ do
      runGEvalTest "multilabel-f1-ie-fuzzy-harden" `shouldReturnAlmost` 0.555555555
    it "information extraction" $ do
      runGEvalTest "multilabel-f1-ie-probs" `shouldReturnAlmost` 0.1111111111
    it "with top confident" $ do
      runGEvalTest "top-confidence" `shouldReturnAlmost` 0.857142857142857
  describe "Mean/MultiLabel-F" $ do
    it "simple" $ do
      runGEvalTest "mean-multilabel-f1-simple" `shouldReturnAlmost` 0.5
  describe "MultiLabel-Likelihood" $ do
    it "simple" $ do
      runGEvalTest "multilabel-likelihood-simple" `shouldReturnAlmost` 0.115829218528827
  describe "Probabilistic-Soft2D-F" $ do
    it "simple" $ do
      runGEvalTest "probabilistic-soft2d-f1-simple" `shouldReturnAlmost` 0.3369517442617539
    it "recall" $ do
      runGEvalTest "probabilistic-soft2d-recall-simple" `shouldReturnAlmost` 0.30717411254418137
  describe "MacroAvg" $ do
    it "with Likelihood" $ do
      runGEvalTest "macroavg-likelihood" `shouldReturnAlmost` 0.500506559087994
  describe "MAE-Against-Interval" $ do
    it "simple" $ do
      runGEvalTest "mae-against-interval-simple" `shouldReturnAlmost` 1.0
  describe "Preprocessing operations" $ do
    it "F1 with preprocessing" $ do
      runGEvalTest "f1-with-preprocessing" `shouldReturnAlmost` 0.57142857142857
    it "BIO-F1 with preprocessing" $ do
      runGEvalTest "bio-f1-flags" `shouldReturnAlmost` 0.75
    it "Regexp substition" $ do
      runGEvalTest "accuracy-with-flags" `shouldReturnAlmost` 0.8
    let sampleChallenge = GEvalSpecification
            { gesOutDirectory = "test/accuracy-flags-line-by-line/accuracy-flags-line-by-line-solution",
              gesExpectedDirectory = Just "test/accuracy-flags-line-by-line/accuracy-flags-line-by-line",
              gesTestName = "test-A",
              gesSelector = Nothing,
              gesOutFile = "out.tsv",
              gesAltOutFiles = Nothing,
              gesExpectedFile = "expected.tsv",
              gesInputFile = "in.tsv",
              gesMetrics = [read "Accuracy:f<in[1]:foo>s<\\d+><>"],
              gesFormatting = FormattingOptions Nothing False,
              gesTokenizer = Just Minimalistic,
              gesGonitoHost = Nothing,
              gesToken = Nothing,
              gesGonitoGitAnnexRemote = Nothing,
              gesReferences = Nothing,
              gesBootstrapResampling = Nothing,
              gesInHeader = Nothing,
              gesOutHeader = Nothing,
              gesShowPreprocessed = False }
    it "In line-by-line mode Accuracy" $ do
      results <- runLineByLineGeneralized KeepTheOriginalOrder sampleChallenge (const Data.Conduit.List.consume)
      results `shouldBe` [
        LineRecord "foo"
                   "Ala 123 ma kota."
                   "Ala ma 2 kota ."
                   1
                   1.0,
        LineRecord "foo"
                   "Foo bar baz"
                   "Fox bax 456 bax"
                   2
                   0.0]
    it "In line-by-line mode F0" $ do
      results <- runLineByLineGeneralized KeepTheOriginalOrder sampleChallenge { gesMetrics = [read "MultiLabel-F0:f<in[1]:foo>s<\\d+><>"]} (const Data.Conduit.List.consume)
      results `shouldBe` [
        LineRecord "foo"
                   "Ala 123 ma kota."
                   "Ala ma 2 kota ."
                   1
                   1.0,
        LineRecord "foo"
                   "Foo bar baz"
                   "Fox bax 456 bax"
                   2
                   0.0]
  describe "Flag examples" $ do
    it "none" $ do
      runGEvalTest "flags-none" `shouldReturnAlmost` 0.2
    it "lower-case" $ do
      runGEvalTest "flags-lowercase" `shouldReturnAlmost` 0.3
    it "upper-case" $ do
      runGEvalTest "flags-uppercase" `shouldReturnAlmost` 0.4
    it "regexp-matching" $ do
      runGEvalTest "flags-regexp-matching" `shouldReturnAlmost` 0.8
    it "regexp-matching-anchor" $ do
      runGEvalTest "flags-regexp-matching-anchor" `shouldReturnAlmost` 0.8
    it "regexp-token-matching" $ do
      runGEvalTest "flags-regexp-token-matching" `shouldReturnAlmost` 0.7
    it "regexp-token-matching-anchor" $ do
      runGEvalTest "flags-regexp-token-matching-anchor" `shouldReturnAlmost` 0.8
    it "regexp-substitution" $ do
      runGEvalTest "flags-regexp-substitution" `shouldReturnAlmost` 0.3
    it "regexp-substitution-ref" $ do
      runGEvalTest "flags-regexp-substitution-ref" `shouldReturnAlmost` 0.5
    it "sort" $ do
      runGEvalTest "flags-sort" `shouldReturnAlmost` 0.3
    it "filtering" $ do
      runGEvalTest "flags-filtering" `shouldReturnAlmost` 0.25
    it "filtering and matching" $ do
      runGEvalTest "flags-filter-and-match" `shouldReturnAlmost` 0.8
  describe "evaluating single lines" $ do
    it "RMSE" $ do
      (MetricOutput (SimpleRun v) _) <- gevalCoreOnSingleLines RMSE id RawItemTarget
                                                          (LineInFile (FilePathSpec "stub1") 1 "blabla")
                                                          RawItemTarget
                                                          (LineInFile (FilePathSpec "stub2") 1 "3.4")
                                                          RawItemTarget
                                                          (LineInFile (FilePathSpec "stub3") 1 "2.6")
      v `shouldBeAlmost` 0.8
  describe "Annotation format" $ do
    it "just parse" $ do
      parseAnnotations "foo:3,7-10 baz:4-6" `shouldBe` Right [Annotation "foo" (IS.fromList [3,7,8,9,10]),
                                                              Annotation "baz" (IS.fromList [4,5,6])]
    it "just parse wit colons" $ do
      parseSegmentAnnotations "foo:x:3,7-10 baz:4-6" `shouldBe` Right [Annotation "foo:x" (IS.fromList [3,7,8,9,10]),
                                                                       Annotation "baz" (IS.fromList [4,5,6])]
    it "just parse wit colons" $ do
      parseSegmentAnnotations "foo:x:3,7-10 baz:2-6" `shouldBe` Left "Overlapping segments"
    it "just parse 2" $ do
      parseAnnotations "inwords:1-3 indigits:5" `shouldBe` Right [Annotation "inwords" (IS.fromList [1,2,3]),
                                                                  Annotation "indigits" (IS.fromList [5])]
    it "empty" $ do
      parseAnnotations "" `shouldBe` Right []
    it "empty (just spaces)" $ do
      parseAnnotations "   " `shouldBe` Right []
    it "match score" $ do
      matchScore (Annotation "x" (IS.fromList [3..6])) (ObtainedAnnotation (Annotation "y" (IS.fromList [3..6])) 1.0) `shouldBeAlmost` 0.0
      matchScore (Annotation "x" (IS.fromList [3..6])) (ObtainedAnnotation (Annotation "x" (IS.fromList [3..6])) 1.0) `shouldBeAlmost` 1.0
      matchScore (Annotation "x" (IS.fromList [123..140])) (ObtainedAnnotation (Annotation "x" (IS.fromList [125..130])) 1.0) `shouldBeAlmost` 0.33333
      matchScore (Annotation "x" (IS.fromList [3..4])) (ObtainedAnnotation (Annotation "x" (IS.fromList [2..13])) 1.0) `shouldBeAlmost` 0.1666666
  describe "BIO format" $ do
    it "just parse" $ do
      let (Right r) = parseOnly (bioSequenceParser <* endOfInput) "O B-city/NEW_YORK I-city B-city/KALISZ I-city O B-name"
      r `shouldBe` [Outside,
                    Beginning "city" (Just "NEW_YORK"),
                    Inside "city" Nothing,
                    Beginning "city" (Just "KALISZ"),
                    Inside "city" Nothing,
                    Outside,
                    Beginning "name" Nothing]
    it "simplest entity" $ do
      let (Right ents) = parseBioSequenceIntoEntities "B-city"
      ents `shouldBe` [TaggedEntity (TaggedSpan 1 1) "city" Nothing]
    it "multi-word entity" $ do
      let (Right ents) = parseBioSequenceIntoEntities "B-date I-date"
      ents `shouldBe` [TaggedEntity (TaggedSpan 1 2) "date" Nothing]
    it "multi-word entity with normalized text" $ do
      let (Right ents) = parseBioSequenceIntoEntities "B-date/FOO I-date/BAR"
      ents `shouldBe` [TaggedEntity (TaggedSpan 1 2) "date" (Just "FOO_BAR")]
    it "simplest entity with something outside" $ do
      let (Right ents) = parseBioSequenceIntoEntities "O B-city"
      ents `shouldBe` [TaggedEntity (TaggedSpan 2 2) "city" Nothing]
    it "another simple case" $ do
      let (Right ents) = parseBioSequenceIntoEntities "B-city B-city"
      ents `shouldBe` [TaggedEntity (TaggedSpan 1 1) "city" Nothing,
                       TaggedEntity (TaggedSpan 2 2) "city" Nothing]
    it "just parse into entities" $ do
      let (Right ents) = parseBioSequenceIntoEntities "O O B-city/LOS_ANGELES I-city B-city/KLUCZBORK O B-name O B-person/JOHN I-person/VON I-person/NEUMANN"
      ents `shouldBe` [TaggedEntity (TaggedSpan 3 4) "city" (Just "LOS_ANGELES"),
                       TaggedEntity (TaggedSpan 5 5) "city" (Just "KLUCZBORK"),
                       TaggedEntity (TaggedSpan 7 7) "name" (Nothing),
                       TaggedEntity (TaggedSpan 9 11) "person" (Just "JOHN_VON_NEUMANN")]
    it "another entity parse" $ do
      let (Right ents) = parseBioSequenceIntoEntities "B-month/JULY B-month/JULY O O B-foo/bar"
      ents `shouldBe` [TaggedEntity (TaggedSpan 1 1) "month" (Just "JULY"),
                       TaggedEntity (TaggedSpan 2 2) "month" (Just "JULY"),
                       TaggedEntity (TaggedSpan 5 5) "foo" (Just "bar")]
    it "another entity parse" $ do
      let (Right ents) = parseBioSequenceIntoEntities "B-city/LOS I-city/ANGELES O B-city/NEW I-city/YORK"
      ents `shouldBe` [TaggedEntity (TaggedSpan 1 2) "city" (Just "LOS_ANGELES"),
                       TaggedEntity (TaggedSpan 4 5) "city" (Just "NEW_YORK")]
    it "parse entity" $ do
      let (Right ents) = parseBioSequenceIntoEntities "B-surname/BROWN B-surname/SMITH"
      ents `shouldBe` [TaggedEntity (TaggedSpan 1 1) "surname" (Just "BROWN"),
                       TaggedEntity (TaggedSpan 2 2) "surname" (Just "SMITH")]
    it "parse entity" $ do
      let (Right ents) = parseBioSequenceIntoEntities "O B-surname/SMITH"
      ents `shouldBe` [TaggedEntity (TaggedSpan 2 2) "surname" (Just "SMITH")]
    it "check counting" $ do
      gatherCountsForBIO [TaggedEntity (TaggedSpan 2 2) "surname" (Just "SMITH")] [TaggedEntity (TaggedSpan 1 1) "surname" (Just "BROWN"),
                                                                                   TaggedEntity (TaggedSpan 2 2) "surname" (Just "SMITH")] `shouldBe` (1, 1, 2)
    it "check F1 on a more complicated example" $ do
      runGEvalTest "bio-f1-complex" `shouldReturnAlmost` 0.625
    it "check F1 on labels only" $ do
      runGEvalTest "bio-f1-complex-labels" `shouldReturnAlmost` 0.6666666666
    it "calculate F1" $ do
      runGEvalTest "bio-f1-simple" `shouldReturnAlmost` 0.5
    it "calculate F1 with underscores rather than minus signs" $ do
      runGEvalTest "bio-f1-simple-underscores" `shouldReturnAlmost` 0.5
    it "check perfect score" $ do
      runGEvalTest "bio-f1-perfect" `shouldReturnAlmost` 1.0
    it "check inconsistent input" $ do
      runGEvalTest "bio-f1-error" `shouldThrow` (== UnexpectedData 2 "inconsistent label sequence `B-NAME/JOHN I-FOO/SMITH I-FOO/X`")
    it "weighted F1" $ do
      runGEvalTest "bio-weighted-f1-simple" `shouldReturnAlmost` 0.82539682
  describe "automatic decompression" $ do
    it "more complex test" $ do
      runGEvalTest "charmatch-complex-compressed" `shouldReturnAlmost` 0.1923076923076923
  describe "headers" $ do
    it "simple" $ do
      runGEvalTestExtraOptions [] "mse-simple-headers" `shouldReturnAlmost` 0.4166666666666667
  describe "handling jsonl format" $ do
    it "simple test" $
      runGEvalTestExtraOptions ["-e", "expected.jsonl" ] "jsonl-simple" `shouldReturnAlmost` 0.571428571428
  describe "line by line mode" $ do
    let sampleChallenge =
          GEvalSpecification
          { gesOutDirectory = "test/likelihood-simple/likelihood-simple-solution",
            gesExpectedDirectory = Just "test/likelihood-simple/likelihood-simple",
            gesTestName = "test-A",
            gesSelector = Nothing,
            gesOutFile = "out.tsv",
            gesAltOutFiles = Nothing,
            gesExpectedFile = "expected.tsv",
            gesInputFile = "in.tsv",
            gesMetrics = [EvaluationScheme Likelihood []],
            gesFormatting = FormattingOptions Nothing False,
            gesTokenizer = Nothing,
            gesGonitoHost = Nothing,
            gesToken = Nothing,
            gesGonitoGitAnnexRemote = Nothing,
            gesReferences = Nothing,
            gesBootstrapResampling = Nothing,
            gesInHeader = Nothing,
            gesOutHeader = Nothing,
            gesShowPreprocessed = False }
    it "simple test" $ do
      results <- runLineByLineGeneralized KeepTheOriginalOrder sampleChallenge (const Data.Conduit.List.consume)
      Prelude.map (\(LineRecord inp _ _ _ _) -> inp) results `shouldBe` ["foo",
                                                                        "bar",
                                                                        "baz",
                                                                        "baq"]
    it "test sorting" $ do
      results <- runLineByLineGeneralized FirstTheWorst sampleChallenge (const Data.Conduit.List.consume)
      Prelude.head (Prelude.map (\(LineRecord inp _ _ _ _) -> inp) results) `shouldBe` "baq"
  describe "handle --alt-metric option" $ do
    it "accuracy instead of likelihood" $ do
      runGEvalTestExtraOptions ["--alt-metric", "Accuracy"] "likelihood-simple" `shouldReturnAlmost` 0.75
    it "accuracy instead of log loss" $ do
      runGEvalTestExtraOptions ["--alt-metric", "Accuracy"] "log-loss-hashed-probs" `shouldReturnAlmost` 0.4
  describe "smart sources" $ do
    it "smart specs are obtained" $ do
      getSmartSourceSpec "foo" "" "" `shouldReturn` Left NoSpecGiven
      getSmartSourceSpec "foo" "out.tsv" "-" `shouldReturn` Right Stdin
      getSmartSourceSpec "foo" "out.sv" "http://gonito.net/foo" `shouldReturn` (Right $ Http "http://gonito.net/foo")
      getSmartSourceSpec "foo" "in.tsv" "https://gonito.net" `shouldReturn` (Right $ Https "https://gonito.net")
    it "sources are accessed" $ do
      readFromSmartSource "baz" "out.tsv" "test/files/foo.txt" `shouldReturn` ["foo\n"]
      readFromSmartSource "" "" "https://httpbin.org/robots.txt" `shouldReturn`
        ["User-agent: *\nDisallow: /deny\n"]
  describe "parse model params from filenames" $ do
    it "no params 1" $ do
      parseParamsFromFilePath "out.tsv" `shouldBe` OutputFileParsed "out" Data.Map.Strict.empty
    it "no params 2" $ do
      parseParamsFromFilePath "out.tsv.xz" `shouldBe` OutputFileParsed "out" Data.Map.Strict.empty
    it "no params 3" $ do
      parseParamsFromFilePath "out-test-foo_bar.tsv" `shouldBe` OutputFileParsed "out-test-foo_bar" Data.Map.Strict.empty
    it "one parameter" $ do
      parseParamsFromFilePath "out-nb_epochs=123.tsv" `shouldBe`
        OutputFileParsed "out" (Data.Map.Strict.fromList [("nb_epochs", "123")])
    it "complex" $ do
      parseParamsFromFilePath "out-nb_epochs = 12,foo=off, bar-baz =10.tsv" `shouldBe`
        OutputFileParsed "out" (Data.Map.Strict.fromList [("nb_epochs", "12"),
                                                          ("foo", "off"),
                                                          ("bar-baz", "10")])
    it "empty val" $ do
      parseParamsFromFilePath "out-nb_epochs=1,foo=,bar-baz=8.tsv" `shouldBe`
        OutputFileParsed "out" (Data.Map.Strict.fromList [("nb_epochs", "1"),
                                                          ("foo", ""),
                                                          ("bar-baz", "8")])
  describe "ranking" $ do
    it "simple case" $ do
      checkConduitPure (rank (\(a,_) (b,_) -> a < b)) [(3.0::Double, "foo"::String),
                                   (10.0, "bar"),
                                   (12.0, "baz")]
                                  [(1.0, (3.0::Double, "foo"::String)),
                                   (2.0, (10.0, "bar")),
                                   (3.0, (12.0, "baz"))]
    it "one item" $ do
      checkConduitPure (rank (\(a,_) (b,_) -> a < b)) [(5.0::Double, "foo"::String)]
                                  [(1.0, (5.0::Double, "foo"::String))]
    it "take between" $ do
      checkConduitPure (rank (<)) [3.0::Double, 5.0, 5.0, 10.0]
                                [(1.0::Double, 3.0),
                                 (2.5, 5.0),
                                 (2.5, 5.0),
                                 (4.0, 10.0)]
    it "two sequences" $ do
      checkConduitPure (rank (<)) [4.5::Double, 4.5, 4.5, 6.1, 6.1]
                                [(2.0::Double, 4.4),
                                 (2.0, 4.5),
                                 (2.0, 4.5),
                                 (4.5, 6.1),
                                 (4.5, 6.1)]
    it "series at the beginning" $ do
      checkConduitPure (rank (<)) [10.0::Double, 10.0, 13.0, 14.0]
                                [(1.5::Double, 10.0),
                                 (1.5, 10.0),
                                 (3.0, 13.0),
                                 (4.0, 14.0)]
    it "inverted" $ do
      checkConduitPure (rank (>)) [3.0::Double, 3.0, 2.0, 1.0]
                                [(1.5::Double, 3.0),
                                 (1.5, 3.0),
                                 (3.0, 2.0),
                                 (4.0, 1.0)]
  describe "bootstrap conduit" $ do
    it "sanity test" $ do
      let nbOfSamples = 1000
      let listChecked :: [Int] = [0..10]

      (runResourceT $ runConduit (CL.sourceList listChecked .| CC.product)) `shouldReturn` 0

      results <- runResourceT $ runConduit (CL.sourceList listChecked .| bootstrapC nbOfSamples CC.product)
      Prelude.length results `shouldBe` nbOfSamples
      (Prelude.length (Prelude.filter (> 0) results)) `shouldNotBe` 0
    it "test gettings bounds" $ do
      let sample = [3.0, 11.0, 2.0, 4.0, 15.0,  12.0, 2013.5, 19.0, 17.0, -10000.0,
                    16.0, 13.0, 6.0, 7.0, 8.0,  5.0, 9.0, 10.0, 14.0, 18]
      getConfidenceBounds defaultConfidenceLevel sample `shouldBe` (-10000.0, 2013.5)
      getConfidenceBounds 0.9 sample `shouldBe` (2.0, 19.0)
  describe "tokenizer" $ do
    it "simple utterance with '13a' tokenizer" $ do
      tokenize (Just V13a) "To be or not to be, that's the question." `shouldBe`
        ["To", "be", "or", "not", "to", "be",
         ",", "that's", "the", "question", "."]
    it "simple utterance with 'character-by-character' tokenizer" $ do
      tokenize (Just CharacterByCharacter) "To be or not to be." `shouldBe`
        ["T", "o", "_", "b", "e", "_", "o", "r", "_", "n", "o", "t", "_", "t", "o", "_", "b", "e", "."]
  describe "shapify" $ do
    it "simple tests" $ do
      shapify "Poznań" `shouldBe` (WordShape "Aa+")
      shapify "2019" `shouldBe` (WordShape "9999")
      shapify "Ala ma (czarnego) kota?" `shouldBe` (WordShape "Aa+ a+ (a+( a+.")
      shapify "" `shouldBe` (WordShape "")
      shapify "PCMCIA" `shouldBe` (WordShape "A+")
      shapify "a" `shouldBe` (WordShape "a")
      shapify "B5" `shouldBe` (WordShape "A9")
  describe "create challenges and validate them" $ do
    (flip mapM_) listOfAvailableEvaluationSchemes $ \scheme -> do
        it (show scheme) $ do
          withSystemTempDirectory "geval-validation-test" $ \tempDir -> do
            let spec = defaultGEvalSpecification {
                  gesExpectedDirectory = Just tempDir,
                  gesMetrics = [scheme],
                  gesFormatting = FormattingOptions (Just 4) False }
            createChallenge True tempDir spec
            validationChallenge tempDir spec
  describe "check validation on broken challenges" $ do
    it "broken metric" $ do
      (hSilence [stderr] $ runGEval ["--validate", "--expected-directory", "test/_validation/broken-metric"]) `shouldThrow` anyException

  describe "test sample outputs" $ do
    (flip mapM_ ) (Prelude.filter isEvaluationSchemeDescribed listOfAvailableEvaluationSchemes) $ \scheme@(EvaluationScheme metric _) -> do
      it (show scheme) $ do
        withSystemTempDirectory "geval-sample-output-test" $ \tempDir -> do
          let spec = defaultGEvalSpecification {
                gesExpectedDirectory = Just tempDir,
                gesMetrics = [scheme] }
          createChallenge True tempDir spec
          let outFile = tempDir </> "test-A" </> "out.tsv"
          writeFile outFile (outContents metric)
          obtainedScore <- (runGEval ["--expected-directory", tempDir, "--out-directory", tempDir]) >>= extractVal
          obtainedScore `shouldBeAlmost` (expectedScore scheme)
  describe "submit" $ do
    it "current branch" $ do
      runGitTest "branch-test" (\_ -> getCurrentBranch) `shouldReturn` "develop"
    it "challengeId" $ do
      runGitTest "challengeId-test" (
        \_ -> do
          path <- makeAbsolute "challenge01"
          setCurrentDirectory path
          getChallengeId) `shouldReturn` "challenge01"
    it "everything committed - positive" $ do
      runGitTest "everythingCommitted-test-pos" (\_ -> checkEverythingCommitted) `shouldReturn` ()
    it "everything committed - negative" $ do
      hSilence [stderr] $ runGitTest "everythingCommitted-test-neg" (\_ -> checkEverythingCommitted) `shouldThrow` (== ExitFailure 1)
    it "remote synced - positive" $ do
      runGitTest "remoteSynced-test-pos" (\_ -> checkRemoteSynced) `shouldReturn` ()
    it "remote synced - negative" $ do
      hSilence [stderr] $ runGitTest "remoteSynced-test-neg" (\_ -> checkRemoteSynced) `shouldThrow` (== ExitFailure 1)
    it "remote url" $ do
      runGitTest "remoteUrl-test" (\_ -> getRemoteUrl "origin") `shouldReturn` "git@git.example.com:example/example.git"
    it "repo root" $ do
      runGitTest "repoRoot-test" (
        \path -> do
          subpath <- makeAbsolute "A/B"
          setCurrentDirectory subpath
          root <- getRepoRoot
          return $ root == path
        ) `shouldReturn` True
    it "no token" $ do
      runGitTest "token-test-no" (\_ -> readToken) `shouldReturn` Nothing
    it "read token" $ do
      runGitTest "token-test-yes" (\_ -> readToken) `shouldReturn` (Just "AAAA")
    it "write-read token" $ do
      runGitTest "token-test-no" (
        \_ -> do
          writeToken "BBBB"
          token <- readToken
          return $ token == (Just "BBBB")
       ) `shouldReturn` True
  describe "extracting features" $ do
    it "extract factors" $ do
      let bbdo = BlackBoxDebuggingOptions {
         bbdoMinFrequency = 1,
         bbdoWordShapes = False,
         bbdoBigrams = True,
         bbdoCartesian = False,
         bbdoMinCartesianFrequency = Nothing,
         bbdoConsiderNumericalFeatures = True }
      (sort $ extractFactorsFromTabbed Nothing bbdo Nothing "in" "I like this\t34.3\ttests" Nothing) `shouldBe` [
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 1))
                      (SimpleExistentialFactor (SimpleAtomicFactor (TextFactor "I"))),
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 1))
                      (SimpleExistentialFactor (SimpleAtomicFactor (TextFactor "like"))),
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 1))
                      (SimpleExistentialFactor (SimpleAtomicFactor (TextFactor "this"))),
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 1))
                      (SimpleExistentialFactor (BigramFactor (TextFactor "I") (TextFactor "like"))),
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 1))
                      (SimpleExistentialFactor (BigramFactor (TextFactor "like") (TextFactor "this"))),
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 1))
                      (NumericalFactor Nothing 11),
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 2))
                      (SimpleExistentialFactor (SimpleAtomicFactor (TextFactor "34.3"))),
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 2))
                      (NumericalFactor (Just 34.3) 4),
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 3))
                      (SimpleExistentialFactor (SimpleAtomicFactor (TextFactor "tests"))),
         PeggedFactor (FeatureTabbedNamespace "in" (ColumnByNumber 3))
                      (NumericalFactor Nothing 5) ]
  describe "Kendall's tau" $ do
    it "tau" $ do
      kendall (V.fromList $ Prelude.zip [12, 2, 1, 12, 2] [1, 4, 7, 1, 0]) `shouldBeAlmost` (-0.47140452079103173)
    it "z" $ do
      kendallZ (V.fromList $ Prelude.zip [12, 2, 1, 12, 2] [1, 4, 7, 1, 0]) `shouldBeAlmost` (-1.0742)
    it "p-value" $ do
      (2 * (cumulative (normalDistr 0.0 1.0) $ kendallZ (V.fromList $ Prelude.zip [12, 2, 1, 12, 2] [1, 4, 7, 1, 0]))) `shouldBeAlmost` 0.2827
  describe "NDCG" $ do
    let sampleRelevanceScores = fromList [("d1", 3.0), ("d2", 2.0), ("d3", 3.0), ("d5", 1.0), ("d6", 2.0), ("d7", 3.0), ("d8", 2.0)]
    it "simple" $ do
      ndcgAt 6 sampleRelevanceScores ["d1", "d2", "d3", "d4", "d5", "d6"] `shouldBeAlmost` 0.785
    it "handle duplicates" $ do
      ndcgAt 6 sampleRelevanceScores ["d1", "d2", "d1", "d3", "d4", "d5", "d6"] `shouldBeAlmost` 0.785
    it "only binary scores" $ do
      binaryNdcgAt 3 ["foo", "bar", "baz", "baq"] ["aaa", "baz", "bbb", "baq"] `shouldBeAlmost` 0.29608
    it "perfect" $ do
      ndcgAt 6 sampleRelevanceScores ["d1", "d7", "d3", "d2", "d8", "d6"] `shouldBeAlmost` 1.0
    it "worst" $ do
      ndcgAt 6 sampleRelevanceScores ["d4", "aaa", "bbb", "ccc", "ddd", "eee"] `shouldBeAlmost` 0.0
    it "no results" $ do
      ndcgAt 6 sampleRelevanceScores [] `shouldBeAlmost` 0.0
    it "none relevant" $ do
      ndcgAt 6 (fromList []) ([] :: [Text]) `shouldBeAlmost` 1.0
    it "none relevant, some given" $ do
      ndcgAt 6 (fromList []) ["foo", "bar"] `shouldBeAlmost` 1.0
  describe "Loess" $ do
    it "simple" $ do
      loess (DVU.fromList [0.2, 0.6, 1.0])
            (DVU.fromList [-0.6, 0.2, 1.0])
            0.4 `shouldBeAlmost` (-0.2)
  describe "Confidence for a line" $ do
    it "simple case" $ do
      totalLineConfidence "foo:0.6" `shouldBeAlmost` 0.6
    it "more than one" $ do
      totalLineConfidence "foo:1.0 bar:0.6 bar:0.5" `shouldBeAlmost` 0.669432952768766
  describe "Calibration" $ do
    it "empty list" $ do
      calibration [] [] `shouldBeAlmost` 1.0
    it "one element" $ do
      calibration [True] [1.0] `shouldBeAlmost` 1.0
      calibration [False] [0.0] `shouldBeAlmost` 1.0
      calibration [True] [0.0] `shouldBeAlmost` 0.0
      calibration [False] [1.0] `shouldBeAlmost` 0.0
      calibration [True] [0.7] `shouldBeAlmost` 0.7
      calibration [True] [0.3] `shouldBeAlmost` 0.3
      calibration [False] [0.7] `shouldBeAlmost` 0.3
      calibration [False] [0.3] `shouldBeAlmost` 0.7
    it "perfect calibration" $ do
      calibration [True, True, False] [0.5, 1.0, 0.5] `shouldBeAlmost` 1.0
    it "totally wrong" $ do
      calibration [True, False] [0.0, 1.0] `shouldBeAlmost` 0.0
      calibration [True, False, False, True, False] [0.0, 1.0, 1.0, 0.5, 0.5] `shouldBeAlmost` 0.0
      calibration [False, True, True, True, True, False, False, True, False] [0.25, 0.25, 0.0, 0.25, 0.25, 1.0, 1.0, 0.5, 0.5] `shouldBeAlmost` 0.0
  describe "Cartesian strings" $ do
    it "singleton" $ do
      (parseCartesianString "foo") `shouldBe` ["foo"]
    it "simple" $ do
      parseCartesianString "a-{foo,bar,baz}-b" `shouldBe` ["a-foo-b", "a-bar-b", "a-baz-b"]
    it "3x2" $ do
      parseCartesianString "a-{foo,bar,baz}-{b,c}" `shouldBe` ["a-foo-b", "a-foo-c", "a-bar-b",
                                                               "a-bar-c", "a-baz-b", "a-baz-c" ]
    it "3x2x3" $ do
      parseCartesianString "{foo,bar,ba}-{b,c}-{0,1,2}x" `shouldBe` ["foo-b-0x", "foo-b-1x", "foo-b-2x",
                                                                      "foo-c-0x", "foo-c-1x", "foo-c-2x",
                                                                      "bar-b-0x", "bar-b-1x", "bar-b-2x",
                                                                      "bar-c-0x", "bar-c-1x", "bar-c-2x",
                                                                      "ba-b-0x", "ba-b-1x", "ba-b-2x",
                                                                      "ba-c-0x", "ba-c-1x", "ba-c-2x" ]
  describe "cross-tabs" $ do
    it "tricky" $ do
      splitIntoCrossTabs ["AAAfoo",
                          "AAAbar", "BBBbar", "CCCbar",
                          "AAAbaz", "BBBbaz", "CCCbaz" ] `shouldBe ` [
                                SingleItem "AAAfoo",
                                CrossTab [Prefix "AAAba", Prefix "BBBba", Prefix "CCCba"] [Suffix "r", Suffix "z"]]
    it "singleton" $ do
      splitIntoCrossTabs ["abababab"] `shouldBe` [SingleItem "abababab"]
    it "too small" $ do
      splitIntoCrossTabs ["aabb", "aacc"] `shouldBe` [SingleItem "aabb", SingleItem "aacc"]
    it "two tables" $ do
      splitIntoCrossTabs ["yABC", "xx00", "yABD", "ZC", "xx11", "yy00", "yy11", "ZD"] `shouldBe` [
                                         CrossTab [Prefix "yAB", Prefix "Z"] [Suffix "C", Suffix "D"],
                                         CrossTab [Prefix "xx", Prefix "yy"] [Suffix "00", Suffix "11"]]
    it "simple" $ do
      splitIntoCrossTabs ["aabsolutely",
                          "aaafoo",
                          "other",
                          "aaabaz",
                          "aaabaq",
                          "bbbfoo",
                          "bbbbaz",
                          "bbbbaq"] `shouldBe` [SingleItem "aabsolutely",
                                                CrossTab [Suffix "foo", Suffix "baz", Suffix "baq"] [Prefix "aaa", Prefix "bbb"],
                                                SingleItem "other"]

checkConduitPure conduit inList expList = do
  let outList = runConduitPure $ CC.yieldMany inList .| conduit .| CC.sinkList
  mapM_ (\(o,e) -> (fst o) `shouldBeAlmost` (fst e)) $ Prelude.zip outList expList

readFromSmartSource :: FilePath -> FilePath -> String -> IO [String]
readFromSmartSource defaultDir defaultFile specS = do
  (Right spec) <- getSmartSourceSpec defaultDir defaultFile specS
  let source = smartSource spec
  contents <- runResourceT $ runConduit (source .| CT.decodeUtf8Lenient .| CL.consume)
  return $ Prelude.map unpack contents

neverMatch :: Char -> Int -> Bool
neverMatch _ _ = False

alwaysMatch :: Char -> Int -> Bool
alwaysMatch _ _ = True

testMatchFun :: Char -> Int -> Bool
testMatchFun 'a' 1 = True
testMatchFun 'a' 2 = True
testMatchFun 'a' 3 = True
testMatchFun 'b' 1 = True
testMatchFun 'c' 1 = True
testMatchFun _ _ = False

extractVal :: (Either (ParserResult GEvalOptions) (Maybe [(SourceSpec, [MetricResult])])) -> IO MetricValue
extractVal (Right (Just ([(_, (SimpleRun val):_)]))) = return val
extractVal (Right (Just ([(_, (BootstrapResampling vals):_)]))) = return (sum vals / fromIntegral (Prelude.length vals))
extractVal (Right Nothing) = return $ error "no metrics???"
extractVal (Right (Just [])) = return $ error "emtpy metric list???"
extractVal (Left result) = do
  handleParseResult result
  return $ error "something wrong"

runGEvalTest testName = do
  r <- runGEvalTestExtraOptions [] testName
  --  _ <- runGEvalTestExtraOptions ["--line-by-line", "-i", "expected.tsv"] testName
  return r

runGEvalTestExtraOptions extraOptions testName = (runGEval ([
  "--expected-directory",
  "test/" ++ testName ++ "/" ++ testName,
  "--out-directory",
  "test/" ++ testName ++ "/" ++ testName ++ "-solution"] ++ extraOptions)) >>= extractVal

extractMetric :: String -> IO (Maybe Metric)
extractMetric testName = do
  result <- getOptions ["--expected-directory", "test/" ++ testName ++ "/" ++ testName]
  return $ case result of
   Left _ -> Nothing
   Right opts -> Just $ gesMainMetric $ geoSpec opts

(@=~?) :: (Show a, AEq a) => a -> a -> HU.Assertion
(@=~?) actual expected = expected =~ actual HU.@? assertionMsg
    where
      assertionMsg = "Expected : " ++ show expected ++
                     "\nActual   : " ++ show actual

shouldBeAlmost got expected = got @=~? expected

shouldReturnAlmost :: (AEq a, Show a, Eq a) => IO a -> a -> Expectation
shouldReturnAlmost action expected = action >>= (@=~? expected)

runGitTest :: String -> (FilePath -> IO a) -> IO a
runGitTest name callback = do
  withSystemTempDirectory "geval-submit-test" $ \temp -> do
    copyFile ("test/_submit-tests/" ++ name ++ ".tar") (temp ++ "/" ++ name ++ ".tar")
    withCurrentDirectory temp $ do
      callCommand $ "tar xf " ++ name ++ ".tar"
    let testRoot = temp ++ "/" ++ name
    withCurrentDirectory testRoot $ do
      callback testRoot
