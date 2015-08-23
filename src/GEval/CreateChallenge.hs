{-# LANGUAGE QuasiQuotes #-}

module GEval.CreateChallenge
       (createChallenge)
       where

import GEval.Core
import qualified System.Directory as D
import Control.Conditional (whenM)

import System.IO
import System.FilePath
import Control.Exception
import Control.Monad.Trans.Resource
import Data.String.Here

createChallenge :: FilePath -> GEvalSpecification -> IO ()
createChallenge expectedDirectory spec = do
  D.createDirectoryIfMissing False expectedDirectory
  createFile (expectedDirectory </> "README.md") readmeMDContents
  createFile (expectedDirectory </> configFileName) $ configContents metric testName
  D.createDirectoryIfMissing False trainDirectory
  createFile (trainDirectory </> "train.tsv") $ trainContents metric
  D.createDirectoryIfMissing False devDirectory
  createFile (devDirectory </> "in.tsv") $ devInContents metric
  createFile (devDirectory </> "expected.tsv") $ devExpectedContents metric
  D.createDirectoryIfMissing False testDirectory
  createFile (testDirectory </> "in.tsv") $ testInContents metric
  createFile (testDirectory </> expectedFile) $ testExpectedContents metric
  where metric = gesMetric spec
        testName = gesTestName spec
        trainDirectory = expectedDirectory </> "train"
        devDirectory = expectedDirectory </> "dev-0"
        testDirectory = expectedDirectory </> testName
        expectedFile = gesExpectedFile spec

createFile :: FilePath -> String -> IO ()
createFile filePath contents = do
  whenM (D.doesFileExist filePath) $ throwM $ FileAlreadyThere filePath
  writeFile filePath contents

readmeMDContents :: String
readmeMDContents = [here|
GEval — sample challenge
========================

This is a sample challenge for Gonito framework. Replace it with
the description of your challenge.

Directory structure
-------------------

* `README.md` — this file
* `config.txt` — configuration file
* `train/` — directory with training data
* `train/train.tsv` — sample train set
* `dev-0/` — directory with dev (test) data
* `dev-0/in.tsv` — input data for the dev set
* `dev-0/expected.tsv` — expected (reference) data for the dev set
* `test-A` — directory with test data
* `test-A/in.tsv` — input data for the test set
* `test-A/expected.tsv` — expected (reference) data for the test set
|]

configContents :: Metric -> String -> String
configContents metric testName = [i|
--metric ${show metric} --test-name ${testName}
|]

trainContents :: Metric -> String
trainContents _ = [hereLit|0.06	0.39	0	0.206
1.00	1.00	1	0.017
317.8	5.20	67	0.048
14.6	19.22	27	0.047
|]

devInContents :: Metric -> String
devInContents _ = [hereLit|0.72	0	0.007
9.54	62	0.054
|]

devExpectedContents :: Metric -> String
devExpectedContents _ = [hereLit|0.82
95.2
|]

testInContents :: Metric -> String
testInContents _ = [hereLit|1.52	2	0.093
30.06	14	0.009
|]

testExpectedContents :: Metric -> String
testExpectedContents _ = [hereLit|0.11
17.2
|]
