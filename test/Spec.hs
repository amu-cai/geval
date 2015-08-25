import Test.Hspec

import GEval.Core
import GEval.OptionsParser
import GEval.BLEU
import Options.Applicative
import qualified Test.HUnit as HU

main :: IO ()
main = hspec $ do
  describe "root mean square error" $ do
    it "simple test" $ do
      geval (defaultGEvalSpecification {gesExpectedDirectory=Just "test/rmse-simple/rmse-simple", gesOutDirectory="test/rmse-simple/rmse-simple-solution"}) `shouldReturnAlmost` 0.64549722436790
  describe "mean square error" $ do
    it "simple test with arguments" $ do
      ((runGEval ["--expected-directory",
                  "test/mse-simple/mse-simple",
                  "--out-directory",
                  "test/mse-simple/mse-simple-solution"]) >>= extractVal) `shouldReturnAlmost` 0.4166666666666667
  describe "BLEU" $ do
    it "trivial example from Wikipedia" $ do
      ((runGEval ["--expected-directory",
                  "test/bleu-trivial/bleu-trivial",
                  "--out-directory",
                  "test/bleu-trivial/bleu-trivial-solution"]) >>= extractVal) `shouldReturnAlmost` 0.0
    it "complex example" $ do
      ((runGEval ["--expected-directory",
                  "test/bleu-complex/bleu-complex",
                  "--out-directory",
                  "test/bleu-complex/bleu-complex-solution"]) >>= extractVal) `shouldReturnAlmost` 0.6211
    it "perfect translation" $ do
      ((runGEval ["--expected-directory",
                  "test/bleu-perfect/bleu-perfect",
                  "--out-directory",
                  "test/bleu-perfect/bleu-perfect-solution"]) >>= extractVal) `shouldReturnAlmost` 1.0000
  describe "precision count" $ do
    it "simple test" $ do
      precisionCount [["Alice", "has", "a", "cat" ]] ["Ala", "has", "cat"] `shouldBe` 2
    it "none found" $ do
      precisionCount [["Alice", "has", "a", "cat" ]] ["for", "bar", "baz"] `shouldBe` 0
    it "multiple values" $ do
      precisionCount [["bar", "bar", "bar", "bar", "foo", "xyz", "foo"]] ["foo", "bar", "foo", "baz", "bar", "foo"] `shouldBe` 4
    it "multiple refs" $ do
      precisionCount [["foo", "baz"], ["bar"], ["baz", "xyz"]]  ["foo", "bar", "foo"] `shouldBe` 2

extractVal :: (Either (ParserResult GEvalOptions) (Maybe MetricValue)) -> IO MetricValue
extractVal (Right (Just val)) = return val

class AEq a where
    (=~) :: a -> a -> Bool

instance AEq Double where
    x =~ y = abs ( x - y ) < (1.0e-4 :: Double)

(@=~?) :: (Show a, AEq a) => a -> a -> HU.Assertion
(@=~?) expected actual  = expected =~ actual HU.@? assertionMsg
    where
      assertionMsg = "Expected : " ++ show expected ++
                     "\nActual   : " ++ show actual

shouldReturnAlmost :: (AEq a, Show a, Eq a) => IO a -> a -> Expectation
shouldReturnAlmost action expected = action >>= (@=~? expected)
