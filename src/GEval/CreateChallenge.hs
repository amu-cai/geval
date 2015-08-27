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
  createFile (expectedDirectory </> "README.md") $ readmeMDContents metric testName
  createFile (expectedDirectory </> configFileName) $ configContents metric testName
  D.createDirectoryIfMissing False trainDirectory
  createFile (trainDirectory </> "train.tsv") $ trainContents metric
  D.createDirectoryIfMissing False devDirectory
  createFile (devDirectory </> "in.tsv") $ devInContents metric
  createFile (devDirectory </> "expected.tsv") $ devExpectedContents metric
  D.createDirectoryIfMissing False testDirectory
  createFile (testDirectory </> "in.tsv") $ testInContents metric
  createFile (testDirectory </> expectedFile) $ testExpectedContents metric
  createFile (expectedDirectory </> ".gitignore") $ gitignoreContents
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

readmeMDContents :: Metric -> String -> String
readmeMDContents _ testName = [i|
GEval — sample challenge
========================

This is a sample challenge for Gonito framework (guessing the mass of a planet using its
orbital period, orbital eccentricity and the number of its moons). Replace it with
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
* `${testName}` — directory with test data
* `${testName}/in.tsv` — input data for the test set
* `${testName}/expected.tsv` — expected (reference) data for the test set
|]

configContents :: Metric -> String -> String
configContents metric testName = [i|
--metric ${show metric} --test-name ${testName}
|]

trainContents :: Metric -> String
trainContents BLEU = [hereLit|alussa loi jumala taivaan ja maan	he mea hanga na te atua i te timatanga te rangi me te whenua
ja maa oli autio ja tyhjä , ja pimeys oli syvyyden päällä	a kahore he ahua o te whenua , i takoto kau ; he pouri ano a runga i te mata o te hohonu
ja jumalan henki liikkui vetten päällä	na ka whakapaho te wairua o te atua i runga i te kare o nga wai
|]

trainContents _ = [hereLit|0.06	0.39	0	0.206
1.00	1.00	1	0.017
317.8	5.20	67	0.048
14.6	19.22	27	0.047
|]

devInContents :: Metric -> String
devInContents BLEU = [hereLit|ja jumala sanoi : " tulkoon valkeus " , ja valkeus tuli
ja jumala näki , että valkeus oli hyvä ; ja jumala erotti valkeuden pimeydestä
|]
devInContents _ = [hereLit|0.72	0	0.007
9.54	62	0.054
|]

devExpectedContents :: Metric -> String
devExpectedContents BLEU = [hereLit|a ka ki te atua , kia marama : na ka marama
a ka kite te atua i te marama , he pai : a ka wehea e te atua te marama i te pouri
|]
devExpectedContents _ = [hereLit|0.82
95.2
|]

testInContents :: Metric -> String
testInContents BLEU = [hereLit|ja jumala kutsui valkeuden päiväksi , ja pimeyden hän kutsui yöksi
ja tuli ehtoo , ja tuli aamu , ensimmäinen päivä
|]

testInContents _ = [hereLit|1.52	2	0.093
30.06	14	0.009
|]

testExpectedContents :: Metric -> String
testExpectedContents BLEU = [hereLit|na ka huaina e te atua te marama ko te awatea , a ko te pouri i huaina e ia ko te po
a ko te ahiahi , ko te ata , he ra kotahi
|]

testExpectedContents _ = [hereLit|0.11
17.2
|]

gitignoreContents :: String
gitignoreContents = "*~\n"
