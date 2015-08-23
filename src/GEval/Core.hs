module GEval.Core
    ( geval,
      gevalCore,
      Metric(..),
      MetricValue,
      GEvalSpecification(..),
      GEvalOptions(..),
      GEvalException(..),
      defaultGEvalSpecification,
      defaultOutDirectory,
      defaultTestName,
      defaultOutFile,
      defaultExpectedFile,
      defaultMetric,
      getExpectedDirectory,
      configFileName
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
import Control.Exception
import Control.Conditional (unlessM)
import qualified System.Directory as D

import System.FilePath
import Data.Maybe

type MetricValue = Double

data Metric = RMSE | MSE | BLEU
              deriving (Show, Read)

defaultOutDirectory = "."
defaultTestName = "test-A"
defaultOutFile = "out.tsv"
defaultExpectedFile = "expected.tsv"

defaultMetric :: Metric
defaultMetric = RMSE

configFileName :: FilePath
configFileName = "config.txt"

data GEvalSpecification = GEvalSpecification
                          { gesOutDirectory :: FilePath,
                            gesExpectedDirectory :: Maybe FilePath,
                            gesTestName :: String,
                            gesOutFile :: String,
                            gesExpectedFile :: String,
                            gesMetric :: Metric }

getExpectedDirectory :: GEvalSpecification -> FilePath
getExpectedDirectory spec = fromMaybe outDirectory $ gesExpectedDirectory spec
                            where outDirectory = gesOutDirectory spec

data GEvalOptions = GEvalOptions
                    { geoInit :: Bool,
                      geoSpec :: GEvalSpecification }


data GEvalException = NoExpectedFile FilePath
                      | NoOutFile FilePath
                      | NoExpectedDirectory FilePath
                      | NoOutDirectory FilePath
                      | NoExpectedTestDirectory FilePath
                      | NoOutTestDirectory FilePath
                      | FileAlreadyThere FilePath

instance Exception GEvalException

instance Show GEvalException where
  show (NoExpectedFile filePath) = somethingWrongWithFilesMessage "No file with the expected results" filePath
  show (NoOutFile filePath) = somethingWrongWithFilesMessage "No file with the test results" filePath
  show (NoExpectedDirectory filePath) = somethingWrongWithFilesMessage "No directory with the expected results" filePath
  show (NoOutDirectory filePath) = somethingWrongWithFilesMessage "No directory with the test results" filePath
  show (NoExpectedTestDirectory filePath) = somethingWrongWithFilesMessage "No test subdirectory with the expected results" filePath
  show (NoOutTestDirectory filePath) = somethingWrongWithFilesMessage "No test subdirectory with the results obtained" filePath
  show (FileAlreadyThere filePath) = somethingWrongWithFilesMessage "File already there" filePath

somethingWrongWithFilesMessage :: String -> FilePath -> String
somethingWrongWithFilesMessage msg filePath = Prelude.concat
                                [ msg, ": `", filePath, "`" ]

defaultGEvalSpecification = GEvalSpecification {
  gesOutDirectory = defaultOutDirectory,
  gesExpectedDirectory = Nothing,
  gesTestName = defaultTestName,
  gesOutFile = defaultOutFile,
  gesExpectedFile = defaultExpectedFile,
  gesMetric = defaultMetric }


geval :: GEvalSpecification -> IO (MetricValue)
geval gevalSpec = do
  unlessM (D.doesDirectoryExist outDirectory) $ throwM $ NoOutDirectory outDirectory
  unlessM (D.doesDirectoryExist expectedDirectory) $ throwM $ NoExpectedDirectory expectedDirectory
  unlessM (D.doesDirectoryExist outTestDirectory) $ throwM $ NoOutTestDirectory outTestDirectory
  unlessM (D.doesDirectoryExist expectedTestDirectory) $ throwM $ NoExpectedTestDirectory expectedTestDirectory
  gevalCore metric expectedFilePath outFilePath
   where expectedFilePath = expectedTestDirectory </> (gesExpectedFile gevalSpec)
         outFilePath = outTestDirectory </> (gesOutFile gevalSpec)
         expectedTestDirectory = expectedDirectory </> testName
         outTestDirectory = outDirectory </> testName
         expectedDirectory = getExpectedDirectory gevalSpec
         outDirectory = gesOutDirectory gevalSpec
         testName = gesTestName gevalSpec
         metric = gesMetric gevalSpec

gevalCore :: Metric -> String -> String -> IO (MetricValue)
gevalCore MSE expectedFilePath outFilePath = do
  unlessM (D.doesFileExist expectedFilePath) $ throwM $ NoExpectedFile expectedFilePath
  unlessM (D.doesFileExist outFilePath) $ throwM $ NoOutFile outFilePath
  runResourceT $
    (getZipSource $ (,)
       <$> ZipSource (items expectedFilePath)
       <*> ZipSource (items outFilePath))
     $$ (CL.map itemError
         =$ averageC)

gevalCore RMSE expectedFilePath outFilePath = do
  mse <- gevalCore MSE expectedFilePath outFilePath
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
