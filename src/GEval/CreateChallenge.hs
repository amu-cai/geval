{-# LANGUAGE QuasiQuotes #-}

module GEval.CreateChallenge
       (createChallenge,
        testExpectedContents)
       where

import GEval.Metric
import GEval.EvaluationScheme
import GEval.Common (GEvalException(..), FormattingOptions(..))
import GEval.Core (GEvalSpecification(..), configFileName, gesMainMetric, defaultTestName)
import GEval.Submit (tokenFileName)
import GEval.MatchingSpecification (MatchingSpecification(ExactMatch))
import qualified System.Directory as D
import Control.Conditional (whenM)
import Data.Maybe (catMaybes)

import System.FilePath
import Control.Monad.Trans.Resource
import Data.String.Here

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Bool

import Text.Printf

createChallenge :: Bool -> FilePath -> GEvalSpecification -> IO ()
createChallenge withDataFiles expectedDirectory spec = do
  D.createDirectoryIfMissing False expectedDirectory
  D.createDirectoryIfMissing False trainDirectory
  D.createDirectoryIfMissing False devDirectory
  D.createDirectoryIfMissing False testDirectory
  createFile (expectedDirectory </> ".gitignore") $ gitignoreContents
  createFile (expectedDirectory </> "README.md") $ readmeMDContents metric testName
  createFile (expectedDirectory </> configFileName) $ configContents metrics format testName
  createHeaderFile expectedDirectory "in-header.tsv" $ inHeaderContents metric
  createHeaderFile expectedDirectory "out-header.tsv" $ outHeaderContents metric
  if withDataFiles
    then
     do
      createTrainFiles metric trainDirectory expectedFile

      createFile (devDirectory </> "in.tsv") $ devInContents metric
      createFile (devDirectory </> expectedFile) $ devExpectedContents metric

      createFile (testDirectory </> "in.tsv") $ testInContents metric
      createFile (testDirectory </> expectedFile) $ testExpectedContents metric

    else
      return ()
  where metric = gesMainMetric spec
        metrics = gesMetrics spec
        format = gesFormatting spec
        testName = gesTestName spec
        trainDirectory = expectedDirectory </> "train"
        devDirectory = expectedDirectory </> "dev-0"
        testDirectory = expectedDirectory </> testName
        expectedFile = gesExpectedFile spec

createHeaderFile :: FilePath -> FilePath -> Maybe [[Char]] -> IO ()
createHeaderFile _ _ Nothing = return ()
createHeaderFile expectedDirectory headerFile (Just fields) = do
  createFile (expectedDirectory </> headerFile) $ (intercalate "\t" fields) ++ "\n"

createTrainFiles :: Metric -> FilePath -> FilePath -> IO ()
createTrainFiles metric@(LogLossHashed _) trainDirectory _ = createSingleTrainFile metric trainDirectory
createTrainFiles metric@(LikelihoodHashed _) trainDirectory _ = createSingleTrainFile metric trainDirectory
createTrainFiles metric@(PerplexityHashed _) trainDirectory _ = createSingleTrainFile metric trainDirectory
createTrainFiles metric trainDirectory expectedFile = do
  createFile (trainDirectory </> "in.tsv") $ trainInContents metric
  createFile (trainDirectory </> expectedFile) $ trainExpectedContents metric

createSingleTrainFile :: Metric -> FilePath -> IO ()
createSingleTrainFile metric trainDirectory =
  createFile (trainDirectory </> "train.tsv") $ trainContents metric

createFile :: FilePath -> String -> IO ()
createFile filePath contents = do
  whenM (D.doesFileExist filePath) $ throwM $ FileAlreadyThere filePath
  writeFile filePath contents

readmeMDContents :: Metric -> String -> String
readmeMDContents (Mean metric) testName = readmeMDContents metric testName
readmeMDContents (MacroAvg metric) testName = readmeMDContents metric testName
readmeMDContents GLEU testName = readmeMDContents BLEU testName
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
readmeMDContents WER testName = readmeMDContents BLEU testName
readmeMDContents CER testName = [i|
GEval simple OCR challenge
==========================

Do OCR.

This is a sample fake challenge for Gonito framework. Replace it with
the description of your challenge.|] ++ (commonReadmeMDContents testName)
readmeMDContents WAR testName = readmeMDContents WER testName
readmeMDContents CAR testName = readmeMDContents CER testName
readmeMDContents Haversine testName = [i|
GEval simple sphere distance
==========================

Compute distance between two points on a sphere given their longitudes and latitudes.

This is a sample fake challenge for Gonito framework. Replace it with
the description of your challenge.|] ++ (commonReadmeMDContents testName)

readmeMDContents (Improvement _) testName = [i|
GEval simple Improvement challenge
==================================

This is a sample fake challenge for the special Improvement@Threshold metric.

Your need to give a number (which can be interpreted as a quality score) for each item.

Your task is to maximize the difference between the mean for the subset of expected values for which
the quality score exceeds the threshold and the mean for all the items.

|] ++ (commonReadmeMDContents testName)

readmeMDContents (Accuracy _) testName = [i|
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

readmeMDContents (MacroFMeasure _) testName = [i|
GEval sample challenge — guess the language of a first name
===========================================================

This is a sample/toy classification challenge for Gonito framework with Macro-F-measure as the metric.
|] ++ (commonReadmeMDContents testName)


readmeMDContents (SoftFMeasure _) testName = [i|
GEval sample challenge — mark numbers
=====================================

This is a sample/toy classification challenge for Gonito framework with Soft-F-measure as the metric.
|] ++ (commonReadmeMDContents testName)

readmeMDContents (ProbabilisticSoftFMeasure _) testName = [i|
GEval sample challenge — mark numbers
=====================================

This is a sample/toy classification challenge for Gonito framework with Probabilistic-Soft-F-measure as the metric.
|] ++ (commonReadmeMDContents testName)

readmeMDContents (ProbabilisticSoft2DFMeasure b) testName = readmeMDContents (Soft2DFMeasure b) testName
readmeMDContents (Soft2DFMeasure _) testName = [i|
Sample challenge for clippings
==============================

Try to give clippings coordinates as close to the ground-truth
clippings as possible.

Format
------

Each clipping found in a corresponding PDF/DjVu file. Each clipping should be given as L:P/X0,Y0,X1,Y1:PROB,
where:

    L — (optional) label
    P — DjVu page number (starting from 1, optional if one-page documents are assumed)
    X0, Y0, X1, Y1 — clipping coordinates (in pixels)
    PROB — (optional) probability

|] ++ (commonReadmeMDContents testName)

readmeMDContents NMI testName = [i|
Cluster proverbs
================

Cluster proverbs for languages.

This is a sample challenge for flat clustering (unsupervised learning challenge).
|] ++ (commonReadmeMDContents testName)

readmeMDContents (LikelihoodHashed b) testname = readmeMDContents (LogLossHashed b) testname
readmeMDContents (PerplexityHashed b) testname = readmeMDContents (LogLossHashed b) testname

readmeMDContents (LogLossHashed _) testName = [i|
GEval sample challenge — language model evaluation
==================================================

Give a probability distribution for words.

This is a sample challenge for evaluating language models.
The metric is average log-loss calculated for 10-bit hashes.

Train file is a just text file (one utterance per line).
In an input file, left and right contexts (TAB-separated) are given.
In an expected file, the word to be guessed is given.

Format of the output files
--------------------------

For each input line, a probability distribution for words in a gap
must be given with either logprobs or probs.

### Logprobs

The distribution could be given with logprobs:

    word1:logprob1 word2:logprob2 ... wordN:logprobN :logprob0

where *logprobi* is the logarithm of the probability for *wordi* and
*logprob0* is the logarithm of the probability mass for all the other
words (it will be spread between all 1024 fingerprint values). If the
respective probabilities do not sum up to 1:

  * if the sum is larger than 0.0 and smaller than 1.0, and no logprob0
    is given, log of the remaining probablity mass will be assigned to logprob0,
  * otherwise they will be normalised with softmax.

Note: the separator here is space, not TAB!

### Probs

Probabilities could be given (instead of logprobs):

  * if **all** values look as probs and **at least value** is positive, we treat
    the values as probs rather then logprobs (single value 0.0 is treated
    as a logprob, i.e. probability 1.0!);
  * if their sum is greater than 1.0, then we normalize simply by dividing by the sum;
  * if the sum is smaller than 1.0 and there is no entry for all the other words,
    we add such an entry for the missing probability mass;
  * if the sum is smaller than 1.0 and there is an entry for all the other words,
    we normalize by dividing by the sum.
|] ++ (commonReadmeMDContents testName)

readmeMDContents CharMatch testName = [i|
GEval sample machine challenge for text transformation
======================================================

Transform from British English into American English.

This is a sample challenge for Gonito framework for CharMarch metric. Replace it with
the description of your challenge.

CharMatch is F0.5-score (precision more important than recall)
for expected corrections (i.e. changes between the input text
and the expected output).

Directory structure
-------------------

* `README.md` — this file
* `config.txt` — configuration file
* `train/` — directory with training data
* `train/train.tsv` — just some test in American English
* `dev-0/` — directory with dev (test) data
* `dev-0/in.tsv` — British input text for the dev set
* `dev-0/expected.tsv` — American reference text for the dev set
* `${testName}` — directory with test data
* `${testName}/in.tsv` — British input data for the test set
* `${testName}/expected.tsv` — American reference text for the test set
|]

readmeMDContents (NDCG _) testName = readmeMDContents MAP testName
readmeMDContents MAP testName = [i|
English word for a Polish word
================================================

Give a (British or American) English equivalent of a Polish word.

The relevant items are separated by TABs (could be just one item) and returned items
should be separated by TABs.

See Christopher D. Manning, Prabhakar Raghavan and Hinrich Schütze,
"Introduction to Information Retrieval", Cambridge University Press, 2008 for
more discussion of the metric.
|] ++ (commonReadmeMDContents testName)

readmeMDContents LogLoss testName = [i|
Give the probability of a positive sentiment
============================================

Give the probability that a sentence expresses a positive sentiment.

This a sample challenge for the log-loss metric.

|] ++ (commonReadmeMDContents testName)

readmeMDContents Likelihood testName = [i|
Give the probability of a positive sentiment
============================================

Give the probability that a sentence expresses a positive sentiment.

This a sample challenge for the likelihood metric.

|] ++ (commonReadmeMDContents testName)

readmeMDContents BIOF1Labels testName = readmeMDContents BIOF1 testName
readmeMDContents BIOWeightedF1 testName = readmeMDContents BIOF1 testName
readmeMDContents BIOF1 testName = [i|
Tag and normalize names
=======================

Tag names in the tokenized text and normalized them.

The output should be given in the BIO format with the normalized forms given after slashes (see
`dev-0/expected.tsv` for an example).

The metric is F1 counted on entities (not labels).
|] ++ (commonReadmeMDContents testName)

readmeMDContents TokenAccuracy testName = [i|
Get part of speech tags for each token
======================================

This is a sample challenge for TokenAccuracy. We just
count the accuracy per token and skip entries marked as "*"
in the expected file.

More than one option separated with semicolons can be given
in the expected file (but not in the output file).

|] ++ (commonReadmeMDContents testName)

readmeMDContents SegmentAccuracy testName = [i|
Segment a sentence and tag with POS tags
========================================

This is a sample, toy challenge for SegmentAccuracy.

For each sentence, give a sequence of POS tags, each one with
its position (1-indexed). For instance, `N:1-10` means a nouns
starting from the beginning (the first character) up to to the tenth
character (inclusively).

|] ++ (commonReadmeMDContents testName)

readmeMDContents (ProbabilisticMultiLabelFMeasure beta) testName = readmeMDContents (MultiLabelFMeasure beta ExactMatch) testName
readmeMDContents (MultiLabelFMeasure _ _) testName = [i|
Tag names and their component
=============================

Tag names and their components (first name/surname) in a text.

Tags:
* person
* surname
* first-name

For each tag a sequence of token IDs separated with commas should be given (after a slash).
|] ++ (commonReadmeMDContents testName)

readmeMDContents MultiLabelLikelihood testName = readmeMDContents MultiLabelLogLoss testName
readmeMDContents MultiLabelLogLoss testName = [i|
Multi-label classification for sentiment
========================================

Guess sentiments for a given text. More than one sentiment (or none) should be given.

The output format is:

    L1:p1 L2:p2 ... Ln:pn

where is L1, L2, ..., Ln are labels and p1, p2, ..., pn -
probabilities for each label (Li:pi are separated with spaces).
Probabilities can be omitted, 1.0 is assumed then. If a label is not
given at all, probability 0.0 is assumed. (But note that returning
0.0/1.0 probabilities is risky, as if you fail, you will be punished
in an infinite manner).
|] ++ (commonReadmeMDContents testName)

readmeMDContents ClippEU testName = [i|
Sample challenge for clipping rectangles
========================================

The metric is ClippEU, i.e. F2-score (F-measure with preference for recall).

Reference format
----------------

(For expected.tsv files.)

Each line describes expected clippings to be found in a corresponding PDF/DjVu file. Each expected clipping is specified as P/X0,Y0,X1,Y1/M, where:

    P — DjVu page number (starting from 1)
    X0, Y0, X1, Y1 — clipping coordinates (in pixels)
    M — margin of error for each direction (in pixels)

Output format
-------------

(for out.tsv files.)

Similar to the reference format, each line describes clippings found in a corresponding PDF/DjVu file. Each clipping should be given as P/X0,Y0,X1,Y1, where:

    P — DjVu page number (starting from 1)
    X0, Y0, X1, Y1 — clipping coordinates (in pixels)

|] ++ (commonReadmeMDContents testName)

readmeMDContents RMSEAgainstInterval testName = readmeMDContents MSEAgainstInterval testName
readmeMDContents MAEAgainstInterval testName = readmeMDContents MSEAgainstInterval testName
readmeMDContents MSEAgainstInterval testName = [i|
Guessing a text year sample challenge
=====================================

Guess the year of a text

Your task is to guess a year of a text. The expected time is given as an interval.
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
* `train/in.tsv` — input data for the train set
* `train/expected.tsv` — expected (reference) data for the train set
* `dev-0/` — directory with dev (test) data
* `dev-0/in.tsv` — input data for the dev set
* `dev-0/expected.tsv` — expected (reference) data for the dev set
* `${testName}` — directory with test data
* `${testName}/in.tsv` — input data for the test set
* `${testName}/expected.tsv` — expected (reference) data for the test set
|]


configContents :: [EvaluationScheme] -> FormattingOptions -> String -> String
configContents schemes format testName =
  unwords $ catMaybes ((Prelude.map (\scheme -> (Just $ "--metric " ++ (show scheme))) schemes)
                       ++ [testNameOpt]
                       ++ (precisionOpt format)
                       ++ [inHeaderOpts, outHeaderOpts])
    where precisionOpt (FormattingOptions m b) = [
            maybe Nothing (Just . printf "--precision %d") m,
            bool Nothing (Just "--show-as-percentage") b ]
          ((EvaluationScheme mainMetric _):_) = schemes
          testNameOpt = if testName /= defaultTestName
                        then
                          (Just (" --test-name " ++ testName))
                        else
                          Nothing
          inHeaderOpts = getHeaderOpts "in-header" inHeaderContents
          outHeaderOpts = getHeaderOpts "out-header" outHeaderContents
          getHeaderOpts opt selector = case selector mainMetric of
            Just _ -> Just (" --" ++ opt ++ " " ++ (opt <.> "tsv"))
            Nothing -> Nothing

-- Originally train content was in one file, to avoid large changes
-- for the time being we are using the original function.

trainInContents :: Metric -> String
trainInContents Haversine = unlines
                            $ map last
                            $ map (splitOn "\t")
                            $ lines
                            $ trainContents Haversine
trainInContents metric = unlines
                         $ map (intercalate "\t")
                         $ map tail
                         $ map (splitOn "\t")
                         $ lines
                         $ trainContents metric

trainExpectedContents :: Metric -> String
trainExpectedContents Haversine = unlines
                                  $ map (intercalate "\t")
                                  $ map (take 2)
                                  $ map (splitOn "\t")
                                  $ lines
                                  $ trainContents Haversine
trainExpectedContents metric = unlines $ map head $ map (splitOn "\t") $ lines $ trainContents metric

trainContents :: Metric -> String
trainContents (Mean metric) = trainContents metric
trainContents (MacroAvg metric) = trainContents metric
trainContents GLEU = trainContents BLEU
trainContents BLEU = [hereLit|alussa loi jumala taivaan ja maan	he mea hanga na te atua i te timatanga te rangi me te whenua
ja maa oli autio ja tyhjä , ja pimeys oli syvyyden päällä	a kahore he ahua o te whenua , i takoto kau ; he pouri ano a runga i te mata o te hohonu
ja jumalan henki liikkui vetten päällä	na ka whakapaho te wairua o te atua i runga i te kare o nga wai
|]
trainContents WER = trainContents BLEU
trainContents CER = [hereLit|Hannibal ad portas	train1.pdf
equo ne credite	train2.pdf
errare humanum est	train3.pdf
|]
trainContents WAR = trainContents WER
trainContents CAR = trainContents CER
trainContents Haversine = [hereLit|30.47547	-90.100911	some text
33.399478	-110.87095	Another text
|]
trainContents (Improvement _) = [hereLit|2.5	baq
1.8	bar
-0.1	foo
3.0	baz
0.5	foo
|]

trainContents (Accuracy _) = [hereLit|Y	10	none	yes
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

trainContents (MacroFMeasure _) = [hereLit|pl	Stanisław
en	John
de	Hans
pl	Wacław
pl	Jan
pl	Kazimierz
en	Matthew
en	Richard
|]
trainContents (ProbabilisticSoftFMeasure b) = trainContents (SoftFMeasure b)
trainContents (SoftFMeasure _) = [hereLit|indigits:8	I have 3 daughters
indigits:1-2 indigits:9-12	12 July 1812
inwords:11-13	I can see two dogs
|]
trainContents NMI = [hereLit|pl	Kto pod kim dołki kopie, ten sam w nie wpada.
en	The pen is mightier than the sword.
pl	Baba z wozu, koniom lżej.
|]
trainContents (LikelihoodHashed b) = trainContents (LogLossHashed b)
trainContents (PerplexityHashed b) = trainContents (LogLossHashed b)
trainContents (LogLossHashed _) = [hereLit|Ala ma psa i kota
Basia ma psa
Nie kupujemy kota w worku
Czesia ma kota
|]
trainContents CharMatch = [hereLit|Camptown ladies sing dis song, Doo-dah! doo-dah!
Camptown race-track five miles long, Oh, doo-dah day!
I come down dah wid my hat caved in, Doo-dah! doo-dah!
I go back home wid a pocket full of tin, Oh, doo-dah day!
|]
trainContents (NDCG _) = trainContents MAP
trainContents MAP = [hereLit|honor	US	honor
honour	GB	honor
titbit	GB	smakołyk
tidbit	US	smakołyk
|]
trainContents Likelihood = trainContents LogLoss
trainContents LogLoss = [hereLit|0.0	Hell, no!!!
0.0	I hate this stuff
1.0	Lekker!!!
0.0	Boring, boring, boring
|]
trainContents BIOF1Labels = trainContents BIOF1
trainContents BIOWeightedF1 = trainContents BIOF1
trainContents BIOF1 = [hereLit|O O O B-surname/BOND O B-firstname/JAMES B-surname/BOND	My name is Bond , James Bond
O O O O O	There is no name here
B-firstname/JOHN B-surname/VON I-surname/NEUMANN	John von Nueman
|]
trainContents TokenAccuracy = [hereLit|* V N	I like cats
* * V * N	I can see the rainbow
|]
trainContents SegmentAccuracy = [hereLit|Art:1-3 N:5-11 V:12-13 A:15-19	The student's smart
N:1-6 N:8-10 V:12-13 A:15-18	Mary's dog is nice
|]
trainContents (ProbabilisticMultiLabelFMeasure beta) = trainContents (MultiLabelFMeasure beta ExactMatch)
trainContents (MultiLabelFMeasure _ _) = [hereLit|I know Mr John Smith	person/3,4,5 first-name/4 surname/5
Steven bloody Brown	person/1,3 first-name/1 surname/3
James and James	first-name/1 firstname/3
|]
trainContents MultiLabelLikelihood = [hereLit|I hate you!	HATE
Love and hate	LOVE HATE
I am sad	SADNESS
I am so sad and hateful	SADNESS HATE
|]
trainContents (ProbabilisticSoft2DFMeasure b) = trainContents (Soft2DFMeasure b)
trainContents (Soft2DFMeasure _) = [hereLit|foo:2/0,0,10,150	foo.djvu
bar:1/30,40,100,1000	bar.djvu
|]
trainContents ClippEU = [hereLit|1/30,40,100,1000/10	bar.djvu
2/30,40,500,600/10	foo.djvu
|]
trainContents RMSEAgainstInterval = trainContents MSEAgainstInterval
trainContents MAEAgainstInterval = trainContents MSEAgainstInterval
trainContents MSEAgainstInterval = [hereLit|1592,1593.9999	Now is the winter of our discontent
1596,1597.9999	All that glisters is not gold.
1600,1601.9999	What a piece of work is a man!
|]
trainContents _ = [hereLit|0.06	0.39	0	0.206
1.00	1.00	1	0.017
317.8	5.20	67	0.048
14.6	19.22	27	0.047
|]

devInContents :: Metric -> String
devInContents (Mean metric) = devInContents metric
devInContents (MacroAvg metric) = devInContents metric
devInContents GLEU = devInContents BLEU
devInContents BLEU = [hereLit|ja jumala sanoi : " tulkoon valkeus " , ja valkeus tuli
ja jumala näki , että valkeus oli hyvä ; ja jumala erotti valkeuden pimeydestä
|]
devInContents WER = devInContents BLEU
devInContents CER = [hereLit|dev1.pdf
dev2.pdf
|]
devInContents WAR = devInContents WER
devInContents CAR = devInContents CER
devInContents (Accuracy _) = [hereLit|-8	none	no
1	mild	no
|]
devInContents NMI = [hereLit|When in Rome, do as the Romans.
Każda pliszka swój ogonek chwali.
When the going gets tough, the tough get going.
|]
devInContents (FMeasure _) = [hereLit|b	b	W	29520	779	-28	-32	a	0	0	0	0	0	0	0	0	0	0
b	b	W	55200	1259	35	9	a	1	0	1	0	0	0	0	0	4000	4000
|]
devInContents (MacroFMeasure _) = [hereLit|Władysław
Steven
Helmut
|]
devInContents (ProbabilisticSoftFMeasure b) = devInContents (SoftFMeasure b)
devInContents (SoftFMeasure _) = [hereLit|I have two kids
7 April 2003
|]
devInContents (LikelihoodHashed b) = devInContents (LogLossHashed b)
devInContents (PerplexityHashed b) = devInContents (LogLossHashed b)
devInContents (LogLossHashed _) = [hereLit|Nie kupuj	w worku
Ona	psa
|]
devInContents CharMatch = [hereLit|honour to organise
nothing to change
time traveller
|]
devInContents (NDCG _) = devInContents MAP
devInContents MAP = [hereLit|US	noc
GB	wózek dziecięcy
GB	wizualizować
|]
devInContents Likelihood = devInContents LogLoss
devInContents LogLoss = [hereLit|Great stuff!
Boring stuff
That's good
|]
devInContents BIOF1Labels = devInContents BIOF1
devInContents BIOWeightedF1 = devInContents BIOF1
devInContents BIOF1 = [hereLit|Adam and Eve
Mr Jan Kowalski
|]
devInContents TokenAccuracy = [hereLit|The cats on the mat
Ala has a cat
|]
devInContents SegmentAccuracy = [hereLit|John is smart
Mary's intelligent
|]
devInContents (ProbabilisticMultiLabelFMeasure beta) = devInContents (MultiLabelFMeasure beta ExactMatch)
devInContents (MultiLabelFMeasure _ _) = [hereLit|Jan Kowalski is here
I see him
Barbara
|]
devInContents MultiLabelLikelihood = devInContents MultiLabelLogLoss
devInContents MultiLabelLogLoss = [hereLit|I am in love
I am a sad hater
|]
devInContents (ProbabilisticSoft2DFMeasure _) = devInContents ClippEU
devInContents (Soft2DFMeasure _) = devInContents ClippEU
devInContents ClippEU = [hereLit|file1.djvu
file2.djvu
|]
devInContents Haversine = [hereLit|Some dev text
|]
devInContents (Improvement _) = [hereLit|foo
baz
bar
foo
|]
devInContents RMSEAgainstInterval = devInContents MSEAgainstInterval
devInContents MAEAgainstInterval = devInContents MSEAgainstInterval
devInContents MSEAgainstInterval = [hereLit|Misery acquaints a man with strange bedfellows.
Nothing can come of nothing.
|]
devInContents _ = [hereLit|0.72	0	0.007
9.54	62	0.054
|]

devExpectedContents :: Metric -> String
devExpectedContents (Mean metric) = devExpectedContents metric
devExpectedContents (MacroAvg metric) = devExpectedContents metric
devExpectedContents GLEU = devExpectedContents BLEU
devExpectedContents BLEU = [hereLit|a ka ki te atua , kia marama : na ka marama
a ka kite te atua i te marama , he pai : a ka wehea e te atua te marama i te pouri
|]
devExpectedContents WER = devExpectedContents BLEU
devExpectedContents CER = [hereLit|et facta est lux
Et tu, Brute?
|]
devExpectedContents WAR = devExpectedContents WER
devExpectedContents CAR = devExpectedContents CER
devExpectedContents (Accuracy _) = [hereLit|N
Y
|]
devExpectedContents (FMeasure _) = [hereLit|0
1
|]
devExpectedContents (MacroFMeasure _) = [hereLit|pl
en
de
|]
devExpectedContents (ProbabilisticSoftFMeasure b) = devExpectedContents (SoftFMeasure b)
devExpectedContents (SoftFMeasure _) = [hereLit|inwords:8-10
indigits:1 indigits:9-12
|]
devExpectedContents NMI = [hereLit|en
pl
en
|]
devExpectedContents (LikelihoodHashed b) = devExpectedContents (LogLossHashed b)
devExpectedContents (PerplexityHashed b) = devExpectedContents (LogLossHashed b)
devExpectedContents (LogLossHashed _) = [hereLit|kota
ma
|]
devExpectedContents CharMatch = [hereLit|honor to organize
nothing to change
time traveler
|]
devExpectedContents (NDCG _) = devExpectedContents MAP
devExpectedContents MAP = [hereLit|night	nite
pram
visualise
|]
devExpectedContents Likelihood = devExpectedContents LogLoss
devExpectedContents LogLoss = [hereLit|1.0
0.0
1.0
|]
devExpectedContents BIOF1Labels = devExpectedContents BIOF1
devExpectedContents BIOWeightedF1 = devExpectedContents BIOF1
devExpectedContents BIOF1 = [hereLit|B-firstname/ADAM O B-firstname/EVE
O B-firstname/JAN B-surname/KOWALSKI
|]
devExpectedContents TokenAccuracy = [hereLit|* N * * N
N V * N
|]
devExpectedContents SegmentAccuracy = [hereLit|N:1-4 V:6-7 A:9-13
N:1-4 V:6-7 A:9-19
|]
devExpectedContents (ProbabilisticMultiLabelFMeasure beta) = devExpectedContents (MultiLabelFMeasure beta ExactMatch)
devExpectedContents (MultiLabelFMeasure _ _) = [hereLit|person/1,2 first-name/1 surname/2

first-name/1
|]
devExpectedContents MultiLabelLikelihood = devExpectedContents MultiLabelLogLoss
devExpectedContents MultiLabelLogLoss = [hereLit|LOVE
SADNESS LOVE
|]
devExpectedContents (ProbabilisticSoft2DFMeasure b) = devExpectedContents (Soft2DFMeasure b)
devExpectedContents (Soft2DFMeasure _) = [hereLit|
foo:10/10,20,30,100 3/0,50,500,500
|]
devExpectedContents ClippEU = [hereLit|
10/10,20,30,100/5 3/0,50,500,500/5
|]
devExpectedContents Haversine = [hereLit|32.812883	-109.625582
|]
devExpectedContents (Improvement _) = [hereLit|0.1
2.0
1.5
-1.0
|]
devExpectedContents RMSEAgainstInterval = devExpectedContents MSEAgainstInterval
devExpectedContents MAEAgainstInterval = devExpectedContents MSEAgainstInterval
devExpectedContents MSEAgainstInterval = [hereLit|1611.0,1611.5
1605,1606.9999
|]
devExpectedContents _ = [hereLit|0.82
95.2
|]

testInContents :: Metric -> String
testInContents (Mean metric) = testInContents metric
testInContents (MacroAvg metric) = testInContents metric
testInContents GLEU = [hereLit|Alicella on musta kissa.
|]
testInContents BLEU = [hereLit|ja jumala kutsui valkeuden päiväksi , ja pimeyden hän kutsui yöksi
ja tuli ehtoo , ja tuli aamu , ensimmäinen päivä
|]
testInContents WER = testInContents BLEU
testInContents CER = [hereLit|test1.pdf
test2.pdf
|]
testInContents WAR = testInContents WER
testInContents CAR = testInContents CER
testInContents (Accuracy _) = [hereLit|2	mild	yes
-5	mild	no
|]
testInContents (FMeasure _) = [hereLit|b	b	W	15210	527	-64	-56	a	0	0	0	0	0	0	0	0	0	0
b	b	N	38060	486	357	189	b	0	0	0	0	0	0	0	0	0	0
|]
testInContents (MacroFMeasure _) = [hereLit|Arkadiusz
Heinrich
Henry
|]
testInContents (ProbabilisticSoftFMeasure b) = testInContents (SoftFMeasure b)
testInContents (SoftFMeasure _) = [hereLit|Nothing
Four sides
|]
testInContents NMI = [hereLit|Fortune favors the bold.
People who live in glass houses should not throw stones.
W marcu, jak w garncu.
A cada necio agrada su porrada.
Kwiecień plecień, bo przeplata trochę zimy, trochę lata.
|]
testInContents (LikelihoodHashed b) = testInContents (LogLossHashed b)
testInContents (PerplexityHashed b) = testInContents (LogLossHashed b)
testInContents (LogLossHashed _) = [hereLit|Ala	ma
Ona ma kota	worku
|]
testInContents CharMatch = [hereLit|paralysed by practise
recognise
nothing
|]
testInContents (NDCG _) = testInContents MAP
testInContents MAP = [hereLit|US	lekki
GB	słoń
US	słoń
|]
testInContents Likelihood = testInContents LogLoss
testInContents LogLoss = [hereLit|That's great, ha, ha, I love it!
Super-duper!!
That is incredibly boring.
|]
testInContents BIOF1Labels = testInContents BIOF1
testInContents BIOWeightedF1 = testInContents BIOF1
testInContents BIOF1 = [hereLit|Alan Tring
No name here
Tarski is NOT here
|]
testInContents TokenAccuracy = [hereLit|I have cats
I know
|]
testInContents SegmentAccuracy = [hereLit|Mary's cat is old
John is young
|]
testInContents (ProbabilisticMultiLabelFMeasure beta) = testInContents (MultiLabelFMeasure beta ExactMatch)
testInContents (MultiLabelFMeasure _ _) = [hereLit|John bloody Smith
Nobody is there
I saw Marketa
|]
testInContents MultiLabelLikelihood = testInContents MultiLabelLogLoss
testInContents MultiLabelLogLoss = [hereLit|I am very sad
I hate
|]
testInContents (ProbabilisticSoft2DFMeasure _) = testInContents ClippEU
testInContents (Soft2DFMeasure _) = testInContents ClippEU
testInContents ClippEU = [hereLit|file3.djvu
file4.djvu
|]
testInContents Haversine = [hereLit|Some test text
Another test text
|]
testInContents (Improvement _) = [hereLit|foo
foo
bar
baz
foo
|]
testInContents RMSEAgainstInterval = testInContents MSEAgainstInterval
testInContents MAEAgainstInterval = testInContents MSEAgainstInterval
testInContents MSEAgainstInterval = [hereLit|What a piece of work is a man!
Beware the ides of March.
|]
testInContents _ = [hereLit|0.72	0	0.007
9.54	62	0.054
|]

testExpectedContents :: Metric -> String
testExpectedContents (Mean metric) = testExpectedContents metric
testExpectedContents (MacroAvg metric) = testExpectedContents metric
testExpectedContents BLEU = [hereLit|na ka huaina e te atua te marama ko te awatea , a ko te pouri i huaina e ia ko te po
a ko te ahiahi , ko te ata , he ra kotahi
|]
testExpectedContents CER = [hereLit|esse est percipi
tabula rasa
|]
testExpectedContents WER = testExpectedContents BLEU
testExpectedContents CAR = testExpectedContents CER
testExpectedContents WAR = testExpectedContents WER
testExpectedContents (Accuracy _) = [hereLit|N
Y
|]
testExpectedContents (FMeasure _) = [hereLit|0
0
|]
testExpectedContents (MacroFMeasure _) = [hereLit|pl
de
en
|]
testExpectedContents (ProbabilisticSoftFMeasure b) = testExpectedContents (SoftFMeasure b)
testExpectedContents (SoftFMeasure _) = [hereLit|
inwords:1-4
|]
testExpectedContents NMI = [hereLit|en
en
pl
es
pl
|]
testExpectedContents (LikelihoodHashed b) = testExpectedContents (LogLossHashed b)
testExpectedContents (PerplexityHashed b) = testExpectedContents (LogLossHashed b)
testExpectedContents (LogLossHashed _) = [hereLit|ma
w
|]
testExpectedContents CharMatch = [hereLit|paralyzed by practice
recognize
nothing
|]
testExpectedContents (NDCG _) = testExpectedContents MAP
testExpectedContents MAP = [hereLit|light	lite
elephant
elephant
|]
testExpectedContents Likelihood = testExpectedContents LogLoss
testExpectedContents LogLoss = [hereLit|1.0
1.0
0.0
|]
testExpectedContents BIOF1Labels = testExpectedContents BIOF1
testExpectedContents BIOWeightedF1 = testExpectedContents BIOF1
testExpectedContents BIOF1 = [hereLit|B-firstname/ALAN B-surname/TURING
O O O
B-surname/TARSKI O O O
|]
testExpectedContents TokenAccuracy = [hereLit|* V N
* V
|]
testExpectedContents SegmentAccuracy = [hereLit|N:1-6 N:8-10 V:12-13 A:15-17
N:1-4 V:6-7 A:9-13
|]
testExpectedContents (ProbabilisticMultiLabelFMeasure beta) = testExpectedContents (MultiLabelFMeasure beta ExactMatch)
testExpectedContents (MultiLabelFMeasure _ _) = [hereLit|person/1,3 first-name/1 surname/3

first-name/3
|]
testExpectedContents MultiLabelLikelihood = testExpectedContents MultiLabelLogLoss
testExpectedContents MultiLabelLogLoss = [hereLit|SADNESS
HATE
|]
testExpectedContents (ProbabilisticSoft2DFMeasure b) = testExpectedContents (Soft2DFMeasure b)
testExpectedContents (Soft2DFMeasure _) = [hereLit|foo:3/0,0,100,100
bar:1/50,50,1000,1000
|]
testExpectedContents ClippEU = [hereLit|3/0,0,100,100/10
1/10,10,1000,1000/10
|]
testExpectedContents GLEU = [hereLit|Alice has a black cat
|]
testExpectedContents Haversine = [hereLit|39.575264	-76.995928
29.949932	-90.070116
|]
testExpectedContents (Improvement _) = [hereLit|0.3
0.5
1.1
4.5
-0.2
|]
testExpectedContents RMSEAgainstInterval = testExpectedContents MSEAgainstInterval
testExpectedContents MAEAgainstInterval = testExpectedContents MSEAgainstInterval
testExpectedContents MSEAgainstInterval = [hereLit|1600,1601.9999
1599.2,1599.4
|]
testExpectedContents _ = [hereLit|0.11
17.2
|]

inHeaderContents :: Metric -> Maybe [String]
inHeaderContents (Mean metric) = inHeaderContents metric
inHeaderContents (MacroAvg metric) = inHeaderContents metric
inHeaderContents GLEU = Nothing
inHeaderContents BLEU = Nothing
inHeaderContents WER = Nothing
inHeaderContents CER = Just ["Filename"]
inHeaderContents WAR = inHeaderContents WER
inHeaderContents CAR = inHeaderContents CER
inHeaderContents (Accuracy _) = Just ["Temperature", "Wind", "Rain"]
inHeaderContents (FMeasure _) = Just ["seismic",
                                      "seismoacoustic",
                                      "shift",
                                      "genergy",
                                      "gpuls",
                                      "gdenergy",
                                      "gdpuls",
                                      "ghazard",
                                      "nbumps",
                                      "nbumps2",
                                      "nbumps3",
                                      "nbumps4",
                                      "nbumps5",
                                      "nbumps6",
                                      "nbumps7",
                                      "nbumps89",
                                      "energy",
                                      "maxenergy",
                                      "class"]
inHeaderContents (MacroFMeasure _) = Just ["FirstName"]
inHeaderContents (ProbabilisticSoftFMeasure b) = inHeaderContents (SoftFMeasure b)
inHeaderContents (SoftFMeasure _) = Just ["Text"]
inHeaderContents NMI = Just ["Utterance"]
inHeaderContents (LikelihoodHashed b) = inHeaderContents (LogLossHashed b)
inHeaderContents (PerplexityHashed b) = inHeaderContents (LogLossHashed b)
inHeaderContents (LogLossHashed _) = Just ["LeftContext", "RightContext"]
inHeaderContents CharMatch = Just ["Text"]
inHeaderContents (NDCG _) = inHeaderContents MAP
inHeaderContents MAP = Just ["Dialect", "PolishPhrase"]
inHeaderContents Likelihood = inHeaderContents LogLoss
inHeaderContents LogLoss = Just ["Text"]
inHeaderContents BIOF1Labels = inHeaderContents BIOF1
inHeaderContents BIOWeightedF1 = inHeaderContents BIOF1
inHeaderContents BIOF1 = Just ["Text"]
inHeaderContents TokenAccuracy = Just ["TokenizedText"]
inHeaderContents SegmentAccuracy = Just ["Segment"]
inHeaderContents (ProbabilisticMultiLabelFMeasure beta) = inHeaderContents (MultiLabelFMeasure beta ExactMatch)
inHeaderContents (MultiLabelFMeasure _ _) = Just ["Text"]
inHeaderContents MultiLabelLikelihood = inHeaderContents MultiLabelLogLoss
inHeaderContents MultiLabelLogLoss = Just ["Utterance"]
inHeaderContents (ProbabilisticSoft2DFMeasure _) = inHeaderContents ClippEU
inHeaderContents (Soft2DFMeasure _) = inHeaderContents ClippEU
inHeaderContents ClippEU = Just ["DjvuFilePath"]
inHeaderContents Haversine = Just ["Text"]
inHeaderContents (Improvement _) = Just ["SomeText"]
inHeaderContents RMSEAgainstInterval = inHeaderContents MSEAgainstInterval
inHeaderContents MAEAgainstInterval = inHeaderContents MSEAgainstInterval
inHeaderContents MSEAgainstInterval = Just ["SomeText"]
inHeaderContents _ = Just ["OrbitalPeriod", "OrbitalEccentricity", "NumberOfMoons"]

outHeaderContents :: Metric -> Maybe [String]
outHeaderContents (Mean metric) = outHeaderContents metric
outHeaderContents (MacroAvg metric) = outHeaderContents metric
outHeaderContents BLEU = Nothing
outHeaderContents GLEU = Nothing
outHeaderContents WER = Nothing
outHeaderContents CER = Just ["OCRedText"]
outHeaderContents WAR = outHeaderContents WER
outHeaderContents CAR = outHeaderContents CER
outHeaderContents (Accuracy _) = Just ["ShouldYouKidForWalk"]
outHeaderContents (FMeasure _) = Just ["IsSeismicBump"]
outHeaderContents (MacroFMeasure _) = Just ["LanguageCode"]
outHeaderContents (ProbabilisticSoftFMeasure b) = outHeaderContents (SoftFMeasure b)
outHeaderContents (SoftFMeasure _) = Just ["NamesFound"]
outHeaderContents NMI = Just ["LanguageCode"]
outHeaderContents (LikelihoodHashed b) = outHeaderContents (LogLossHashed b)
outHeaderContents (PerplexityHashed b) = outHeaderContents (LogLossHashed b)
outHeaderContents (LogLossHashed _) = Just ["GuessedWord"]
outHeaderContents CharMatch = Just ["NormalizedText"]
outHeaderContents (NDCG _) = Nothing
outHeaderContents MAP = Nothing
outHeaderContents Likelihood = outHeaderContents LogLoss
outHeaderContents LogLoss = Just ["Probability"]
outHeaderContents BIOF1Labels = outHeaderContents BIOF1
outHeaderContents BIOWeightedF1 = outHeaderContents BIOF1
outHeaderContents BIOF1 = Just ["BIOOutput"]
outHeaderContents TokenAccuracy = Just ["PartsOfSpeech"]
outHeaderContents SegmentAccuracy = Just ["PartsOfSpeech"]
outHeaderContents (ProbabilisticMultiLabelFMeasure beta) = outHeaderContents (MultiLabelFMeasure beta ExactMatch)
outHeaderContents (MultiLabelFMeasure _ _) = Just ["Entities"]
outHeaderContents MultiLabelLikelihood = outHeaderContents MultiLabelLogLoss
outHeaderContents MultiLabelLogLoss = Just ["Emotion"]
outHeaderContents (ProbabilisticSoft2DFMeasure b) = outHeaderContents (Soft2DFMeasure b)
outHeaderContents (Soft2DFMeasure _) = Just ["Rectangle"]
outHeaderContents ClippEU = Just ["Rectangle"]
outHeaderContents Haversine = Just ["Longitude", "Latitude"]
outHeaderContents (Improvement _) = Just ["SomeNumber"]
outHeaderContents RMSEAgainstInterval = outHeaderContents MSEAgainstInterval
outHeaderContents MAEAgainstInterval = outHeaderContents MSEAgainstInterval
outHeaderContents MSEAgainstInterval = Just ["FractionalYear"]
outHeaderContents _ = Just ["Mass"]

gitignoreContents :: String
gitignoreContents = [hereLit|
*~
*.swp
*.bak
*.pyc
*.o
.DS_Store
|] ++ tokenFileName ++ "\n"
