module GEval
    ( geval,
      gevalCore,
      Metric(..),
      GEvalSpecification(..),
      GEvalOptions(..),
      defaultGEvalSpecification,
      defaultOutDirectory,
      defaultTestName,
      defaultOutFile,
      defaultExpectedFile,
      defaultMetric
    ) where

import Data.Conduit
import Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import Data.Text
import Data.Text.Read as TR
import Control.Applicative

import System.FilePath
import Data.Maybe

data Metric = MSE | BLEU
              deriving (Show, Read)

defaultOutDirectory = "."
defaultTestName = "test-A"
defaultOutFile = "out.tsv"
defaultExpectedFile = "expected.tsv"

defaultMetric :: Metric
defaultMetric = MSE


data GEvalSpecification = GEvalSpecification
                          { gesOutDirectory :: String,
                            gesExpectedDirectory :: Maybe String,
                            gesTestName :: String,
                            gesOutFile :: String,
                            gesExpectedFile :: String,
                            gesMetric :: Metric }

data GEvalOptions = GEvalOptions
                    { geoInit :: Bool,
                      geoSpec :: GEvalSpecification }


defaultGEvalSpecification = GEvalSpecification {
  gesOutDirectory = defaultOutDirectory,
  gesExpectedDirectory = Nothing,
  gesTestName = defaultTestName,
  gesOutFile = defaultOutFile,
  gesExpectedFile = defaultExpectedFile,
  gesMetric = defaultMetric }


geval :: GEvalSpecification -> IO (Double)
geval gevalSpec = gevalCore metric expectedFilePath outFilePath
   where expectedFilePath = expectedDirectory </> testName </> (gesExpectedFile gevalSpec)
         outFilePath = outDirectory </> testName </> (gesOutFile gevalSpec)
         expectedDirectory = fromMaybe outDirectory $ gesExpectedDirectory gevalSpec
         outDirectory = gesOutDirectory gevalSpec
         testName = gesTestName gevalSpec
         metric = gesMetric gevalSpec

gevalCore :: Metric -> String -> String -> IO (Double)
gevalCore MSE expectedFilePath outFilePath = do
  mse <- runResourceT $
    (getZipSource $ (,)
       <$> ZipSource (items expectedFilePath)
       <*> ZipSource (items outFilePath))
     $$ (CL.map itemError
         =$ averageC)
  return $ mse ** 0.5

averageC :: MonadResource m => Sink Double m Double
averageC = getZipSink
    $ (\total count -> total / fromIntegral count)
  <$> ZipSink CC.sum
  <*> ZipSink CC.length

items :: MonadResource m => String -> Source m Double
items filePath =
  CB.sourceFile filePath
  $= (CT.decode CT.utf8
      =$= CT.lines
      =$= CL.map TR.double
      =$= CC.map getValue)


itemError :: (Double, Double) -> Double
itemError (exp, out) = (exp-out)**2

getValue :: Either String (Double, Text) -> Double
getValue (Right (x, _)) = x
