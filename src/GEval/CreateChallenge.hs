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
readmeMDContents BLEU testName = [i|
GEval sample machine translation challenge
==========================================

Translate from Finnish to Māori.

This is a sample challenge (translating from Finnish to Māori) for Gonito framework. Replace it with
the description of your challenge.

Directory structure
-------------------

* `README.md` — this file
* `config.txt` — configuration file
* `train/` — directory with training data
* `train/train.tsv` — sample parallel corpus (Finnish text in the first column, Māori text in the second one)
* `dev-0/` — directory with dev (test) data
* `dev-0/in.tsv` — Finnish input text for the dev set
* `dev-0/expected.tsv` — Māori reference translation for the dev set
* `${testName}` — directory with test data
* `${testName}/in.tsv` — Finnish input data for the test set
* `${testName}/expected.tsv` — Māori reference translation for the test set
|]

readmeMDContents Accuracy testName = [i|
GEval sample classification challenge
=====================================

Guess whether the weather is good for a walk (given temperature,
wind and rain).

This is a sample/toy classification challenge for Gonito framework. Replace it with
the description of your challenge.
|] ++ (commonReadmeMDContents testName)

readmeMDContents (FMeasure _) testName = [i|
GEval sample challenge — forecast high energy seismic bumps
===========================================================

Based on data set provided by M. Sikora and L. Wróbel, see
https://archive.ics.uci.edu/ml/machine-learning-databases/00266/seismic-bumps.arff

This is a sample/toy classification challenge for Gonito framework with F-measure as the metric.
Replace it with the description of your challenge.

For F-measure the expected value is either 1 or 0 (1 means something you are expected to detect occurred).
The output value could a probability where value greater than or equal to 0.5 is interpreted
as 1.
|] ++ (commonReadmeMDContents testName)

readmeMDContents _ testName = [i|
GEval sample challenge
======================

Guess the mass of a planet.

This is a sample challenge for Gonito framework (guessing the mass of a planet using its
orbital period, orbital eccentricity and the number of its moons). Replace it with
the description of your challenge.
|] ++ (commonReadmeMDContents testName)

commonReadmeMDContents testName = [i|
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
configContents metric testName = "--metric " ++
                                 (show metric) ++
                                 (if testName /= defaultTestName
                                     then
                                        " --test-name " ++ testName
                                     else
                                        "")


trainContents :: Metric -> String
trainContents BLEU = [hereLit|alussa loi jumala taivaan ja maan	he mea hanga na te atua i te timatanga te rangi me te whenua
ja maa oli autio ja tyhjä , ja pimeys oli syvyyden päällä	a kahore he ahua o te whenua , i takoto kau ; he pouri ano a runga i te mata o te hohonu
ja jumalan henki liikkui vetten päällä	na ka whakapaho te wairua o te atua i runga i te kare o nga wai
|]

trainContents Accuracy = [hereLit|Y	10	none	yes
N	-2	strong	no
Y	-3	mild	no
N	-1	mild	yes
N	-10	none	no
Y	-7	none	no
N	-6	mild	no
N	-6	none	no
|]

trainContents (FMeasure _) = [hereLit|0	b	b	W	289580	1986	-38	2	a	2	0	1	1	0	0	0	0	54000	50000
1	b	a	W	577770	2765	27	38	a	1	0	1	0	0	0	0	0	2000	2000
0	b	a	W	347400	1684	-28	-22	a	2	0	1	1	0	0	0	0	31000	30000
0	b	a	N	72370	581	-79	-70	a	0	0	0	0	0	0	0	0	0	0
0	b	a	N	59210	440	-82	-76	a	1	0	1	0	0	0	0	0	2000	2000
0	a	a	N	42560	379	-73	-57	a	1	0	1	0	0	0	0	0	4000	4000
1	a	a	W	268170	1352	-41	-35	a	1	1	0	0	0	0	0	0	400	400
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
devInContents Accuracy = [hereLit|-8	none	no
1	mild	no
|]
devInContents (FMeasure _) = [hereLit|b	b	W	29520	779	-28	-32	a	0	0	0	0	0	0	0	0	0	0
b	b	W	55200	1259	35	9	a	1	0	1	0	0	0	0	0	4000	4000
|]

devInContents _ = [hereLit|0.72	0	0.007
9.54	62	0.054
|]

devExpectedContents :: Metric -> String
devExpectedContents BLEU = [hereLit|a ka ki te atua , kia marama : na ka marama
a ka kite te atua i te marama , he pai : a ka wehea e te atua te marama i te pouri
|]
devExpectedContents Accuracy = [hereLit|N
Y
|]
devExpectedContents (FMeasure _) = [hereLit|0
1
|]
devExpectedContents _ = [hereLit|0.82
95.2
|]

testInContents :: Metric -> String
testInContents BLEU = [hereLit|ja jumala kutsui valkeuden päiväksi , ja pimeyden hän kutsui yöksi
ja tuli ehtoo , ja tuli aamu , ensimmäinen päivä
|]
testInContents Accuracy = [hereLit|2	mild	yes
-5	mild	no
|]
testInContents (FMeasure _) = [hereLit|b	b	W	15210	527	-64	-56	a	0	0	0	0	0	0	0	0	0	0
b	b	N	38060	486	357	189	b	0	0	0	0	0	0	0	0	0	0
|]
testInContents _ = [hereLit|1.52	2	0.093
30.06	14	0.009
|]

testExpectedContents :: Metric -> String
testExpectedContents BLEU = [hereLit|na ka huaina e te atua te marama ko te awatea , a ko te pouri i huaina e ia ko te po
a ko te ahiahi , ko te ata , he ra kotahi
|]
testExpectedContents Accuracy = [hereLit|N
Y
|]
testExpectedContents (FMeasure _) = [hereLit|0
0
|]
testExpectedContents _ = [hereLit|0.11
17.2
|]

gitignoreContents :: String
gitignoreContents = "*~\n"
