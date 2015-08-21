import Test.Hspec

import GEval
import qualified Test.HUnit as HU

main :: IO ()
main = hspec $ do
  describe "mean square error" $ do
    it "simple test" $ do
      geval (defaultGEvalSpecification {gesExpectedDirectory=Just "test/mse-simple/mse-simple", gesOutDirectory="test/mse-simple/mse-simple-solution"}) `shouldReturnAlmost` 0.64549722436790

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
