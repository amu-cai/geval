import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "mean square error" $ do
    it "simple test" $ do
      geval "test/mse-simple/mse-simple/test-A/expected.tsv" "test/mse-simple/mse-simple-solution/test-A/out.tsv" `shouldReturn` 1.118033988749
