import Test.Hspec

import GEval.Core
import GEval.OptionsParser
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

extractVal :: (Either (ParserResult GEvalOptions) (Maybe MetricValue)) -> IO MetricValue
extractVal (Right (Just val)) = return val

class AEq a where
    (=~) :: a -> a -> Bool

instance AEq Double where
    x =~ y = abs ( x - y ) < (1.0e-8 :: Double)

(@=~?) :: (Show a, AEq a) => a -> a -> HU.Assertion
(@=~?) expected actual  = expected =~ actual HU.@? assertionMsg
    where
      assertionMsg = "Expected : " ++ show expected ++
                     "\nActual   : " ++ show actual

shouldReturnAlmost :: (AEq a, Show a, Eq a) => IO a -> a -> Expectation
shouldReturnAlmost action expected = action >>= (@=~? expected)
