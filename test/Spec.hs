{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import GEval.Core
import GEval.OptionsParser
import GEval.BLEU
import GEval.ClippEU
import GEval.PrecisionRecall
import GEval.ClusteringMetrics
import Data.Attoparsec.Text
import Options.Applicative
import Data.Text
import qualified Test.HUnit as HU

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
      geval (defaultGEvalSpecification {gesExpectedDirectory=Just "test/rmse-simple/rmse-simple", gesOutDirectory="test/rmse-simple/rmse-simple-solution"}) `shouldReturnAlmost` 0.64549722436790
  describe "mean square error" $ do
    it "simple test with arguments" $
      runGEvalTest "mse-simple" `shouldReturnAlmost` 0.4166666666666667
  describe "BLEU" $ do
    it "trivial example from Wikipedia" $
      runGEvalTest "bleu-trivial" `shouldReturnAlmost` 0.0
    it "complex example" $
      runGEvalTest "bleu-complex" `shouldReturnAlmost` 0.6211
    it "perfect translation" $
      runGEvalTest "bleu-perfect" `shouldReturnAlmost` 1.0000
  describe "Accuracy" $ do
    it "simple example" $
      runGEvalTest "accuracy-simple" `shouldReturnAlmost` 0.6
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
      runGEvalTest "unexpected-data" `shouldThrow` (== UnexpectedData "input does not start with a digit")
    it "unwanted data is handled" $
      runGEvalTest "unwanted-data" `shouldThrow` (== UnexpectedData "number expected")
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
  describe "ClippEU" $ do
    it "parsing rectangles" $ do
      let (Right r) = parseOnly (lineClippingsParser <* endOfInput) "2/0,0,2,3 10/20,30,40,50 18/0,1,500,3 "
      r `shouldBe` [Clipping (PageNumber 2) (Rectangle (Point 0 0) (Point 2 3)),
                    Clipping (PageNumber 10) (Rectangle (Point 20 30) (Point 40 50)),
                    Clipping (PageNumber 18) (Rectangle (Point 0 1) (Point 500 3))]
    it "no rectangles" $ do
      let (Right r) = parseOnly (lineClippingsParser <* endOfInput) ""
      r `shouldBe` []
    it "just spaces" $ do
      let (Right r) = parseOnly lineClippingsParser "     "
      r `shouldBe` []
    it "parsing specs" $ do
      let (Right r) = parseOnly lineClippingSpecsParser  " 2/0,0,2,3/5  10/20,30,40,50/10"
      r `shouldBe` [ClippingSpec (PageNumber 2) (Rectangle (Point 5 5) (Point 0 0))
                                                (Rectangle (Point 0 0) (Point 7 8)),
                    ClippingSpec (PageNumber 10) (Rectangle (Point 30 40) (Point 30 40))
                                                 (Rectangle (Point 10 20) (Point 50 60))]
    it "full test" $ do
      runGEvalTest "clippeu-simple" `shouldReturnAlmost` 0.399999999999
  describe "evaluation metric specification is parsed" $ do
    it "for simple names" $ do
      let metrics = [RMSE, MSE, BLEU, Accuracy, ClippEU]
      let parsedMetrics = Prelude.map (read . show) metrics
      metrics `shouldBe` parsedMetrics
    it "for F-Measure" $ do
      read "F2" `shouldBe` (FMeasure 2.0)
      read "F1" `shouldBe` (FMeasure 1.0)
      read "F0.5" `shouldBe` (FMeasure 0.5)


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

extractVal :: (Either (ParserResult GEvalOptions) (Maybe MetricValue)) -> IO MetricValue
extractVal (Right (Just val)) = return val

runGEvalTest testName = (runGEval [
  "--expected-directory",
  "test/" ++ testName ++ "/" ++ testName,
  "--out-directory",
  "test/" ++ testName ++ "/" ++ testName ++ "-solution"]) >>= extractVal

extractMetric :: String -> IO (Maybe Metric)
extractMetric testName = do
  result <- getOptions ["--expected-directory", "test/" ++ testName ++ "/" ++ testName]
  return $ case result of
   Left _ -> Nothing
   Right opts -> Just $ gesMetric $ geoSpec opts

class AEq a where
    (=~) :: a -> a -> Bool

instance AEq Double where
    x =~ y = abs ( x - y ) < (1.0e-4 :: Double)

(@=~?) :: (Show a, AEq a) => a -> a -> HU.Assertion
(@=~?) actual expected = expected =~ actual HU.@? assertionMsg
    where
      assertionMsg = "Expected : " ++ show expected ++
                     "\nActual   : " ++ show actual

shouldBeAlmost got expected = got @=~? expected

shouldReturnAlmost :: (AEq a, Show a, Eq a) => IO a -> a -> Expectation
shouldReturnAlmost action expected = action >>= (@=~? expected)
