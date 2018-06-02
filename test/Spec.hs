{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import GEval.Core
import GEval.OptionsParser
import GEval.BLEU
import GEval.ClippEU
import GEval.PrecisionRecall
import GEval.ClusteringMetrics
import GEval.BIO
import GEval.LineByLine
import Data.Attoparsec.Text
import Options.Applicative
import Data.Text
import Text.EditDistance

import Data.Conduit.List (consume)

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
    it "empty translation" $
      runGEvalTest "bleu-empty" `shouldReturnAlmost` 0.0000
  describe "Accuracy" $ do
    it "simple example" $
      runGEvalTest "accuracy-simple" `shouldReturnAlmost` 0.6
    it "with probs" $
      runGEvalTest "accuracy-probs" `shouldReturnAlmost` 0.4
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
  describe "LogLoss" $ do
    it "simple" $ do
      runGEvalTest "logloss-simple" `shouldReturnAlmost` 0.31824
    it "perfect" $ do
      runGEvalTest "logloss-perfect" `shouldReturnAlmost` 0.0
  describe "Likelihood" $ do
    it "simple" $ do
      runGEvalTest "likelihood-simple" `shouldReturnAlmost` 0.72742818469866
  describe "evaluating single lines" $ do
    it "RMSE" $ do
      gevalCoreOnSingleLines RMSE (LineInFile "stub1" 1 "blabla")
                                  (LineInFile "stub2" 1 "3.4")
                                  (LineInFile "stub3" 1 "2.6") `shouldReturnAlmost` 0.8
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
  describe "automatic decompression" $ do
    it "more complex test" $ do
      runGEvalTest "charmatch-complex-compressed" `shouldReturnAlmost` 0.1923076923076923
  describe "line by line mode" $ do
    let sampleChallenge =
          GEvalSpecification
          { gesOutDirectory = "test/likelihood-simple/likelihood-simple-solution",
            gesExpectedDirectory = Just "test/likelihood-simple/likelihood-simple",
            gesTestName = "test-A",
            gesOutFile = "out.tsv",
            gesExpectedFile = "expected.tsv",
            gesInputFile = "in.tsv",
            gesMetric = Likelihood,
            gesPrecision = Nothing }
    it "simple test" $ do
      results <- runLineByLineGeneralized KeepTheOriginalOrder sampleChallenge Data.Conduit.List.consume
      Prelude.map (\(LineRecord inp _ _ _ _) -> inp) results `shouldBe` ["foo",
                                                                        "bar",
                                                                        "baz",
                                                                        "baq"]
    it "test sorting" $ do
      results <- runLineByLineGeneralized FirstTheWorst sampleChallenge Data.Conduit.List.consume
      Prelude.head (Prelude.map (\(LineRecord inp _ _ _ _) -> inp) results) `shouldBe` "baq"
  describe "handle --alt-metric option" $ do
    it "accuracy instead of likelihood" $ do
      runGEvalTestExtraOptions ["--alt-metric", "Accuracy"] "likelihood-simple" `shouldReturnAlmost` 0.75


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

runGEvalTest = runGEvalTestExtraOptions []

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
