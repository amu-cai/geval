module GEval.PolevalQuestionAnswering
    ( polevalQACond
    , nLevenshteinPoleval
    , getNLevenshteinSingleLine
    , f1Answerability
    , getAnswerabilitySingleLine
    , polevalAnswerabilityCond
    ) where


import           Data.Array
import           Data.Conduit             (ConduitM)
import           Data.Conduit.Combinators (foldl)
import           Data.List.Split          (splitOn)
import           GHC.Float                (int2Double)


data AggregatedResult = AggregatedResult
    { sumAgg :: Double
    , avgAgg :: Double
    , nLines :: Int
    }

aggregatedResultZero = AggregatedResult
    { sumAgg = 0.0
    , avgAgg = 0.0
    , nLines = 0
    }


getNLevenshteinSingleLine :: String -> String -> (Bool, Double)
getNLevenshteinSingleLine expected output
    | expected == "" = (False, 0.0)
    | otherwise = (True, metricValue)
    where
        maxLength = int2Double $ maximum [length expected, length output]
        metricValue = 1.0 - ((distance expected output) / maxLength)


distance :: Eq e => [e] -> [e] -> Double
distance u v = int2Double $ memo ! (m, n)
   where memo = listArray ((0, 0), (m, n)) [dist i j | i <- [0..m], j <- [0..n]]

         dist 0 j = j
         dist i 0 = i
         dist i j = minimum [
           1 + memo ! (i, j-1),
           1 + memo ! (i-1, j),
           fromEnum (u' ! (i-1) /= v' ! (j-1)) + memo ! (i-1, j-1) ]

         u' = listArray (0, m-1) u
         v' = listArray (0, n-1) v

         m = length u
         n = length v


polevalQACond :: Monad m => ConduitM (Bool, Double) o m AggregatedResult
polevalQACond = Data.Conduit.Combinators.foldl polevalQACount aggregatedResultZero


polevalQACount :: AggregatedResult -> (Bool, Double) -> AggregatedResult
polevalQACount agg (cond, num)
    | cond = AggregatedResult
        { sumAgg = newSum
        , avgAgg = newSum / (int2Double newNLines)
        , nLines = newNLines
        }
    | otherwise = agg
    where
        newSum = sumAgg agg + num
        newNLines = succ $ nLines agg


nLevenshteinPoleval :: AggregatedResult -> Double
nLevenshteinPoleval = avgAgg


data AnswerabilityScore = TP | TN | FP | FN
    deriving (Eq, Show)


data AnswerabilityAgg = AnswerabilityAgg
    { truePostivie  :: Int -- pusty string w expected.tsv i pusty string w out.tsv
    , trueNegative  :: Int -- niepusty string w expected.tsv i niepusty string w out.tsv
    , falsePositive :: Int -- niepusty string w expected.tsv i pusty string w out.tsv
    , falseNegative :: Int -- pusty string w expected.tsv i niepusty string w out.tsv
    } deriving (Eq, Show)


answerabilityAggZero :: AnswerabilityAgg
answerabilityAggZero = AnswerabilityAgg
    { truePostivie  = 0
    , trueNegative  = 0
    , falsePositive = 0
    , falseNegative = 0
    }


getAnswerabilitySingleLine :: String -> String -> AnswerabilityScore
getAnswerabilitySingleLine expected output
    | expected == "" && output == "" = TP
    | expected == "" && output /= "" = FN
    | expected /= "" && output /= "" = TN
    | otherwise                      = FP


countAnswerability :: AnswerabilityAgg -> AnswerabilityScore -> AnswerabilityAgg
countAnswerability (AnswerabilityAgg tpOld tnOld fpOld fnOld) sc
    | sc == TP = AnswerabilityAgg
        { truePostivie  = succ tpOld
        , trueNegative  = tnOld
        , falsePositive = fpOld
        , falseNegative = fnOld
        }
    | sc == FN = AnswerabilityAgg
        { truePostivie  = tpOld
        , trueNegative  = tnOld
        , falsePositive = fpOld
        , falseNegative = succ fnOld
        }
    | sc == TN = AnswerabilityAgg
        { truePostivie  = tpOld
        , trueNegative  = succ tnOld
        , falsePositive = fpOld
        , falseNegative = fnOld
        }
    | sc == FP = AnswerabilityAgg
        { truePostivie  = tpOld
        , trueNegative  = tnOld
        , falsePositive = succ fpOld
        , falseNegative = fnOld
        }


polevalAnswerabilityCond :: Monad m => ConduitM AnswerabilityScore o m AnswerabilityAgg
polevalAnswerabilityCond = Data.Conduit.Combinators.foldl countAnswerability answerabilityAggZero


f1Answerability :: AnswerabilityAgg -> Double
f1Answerability (AnswerabilityAgg tp _ fp fn) = tpD / (tpD + ((fpD + fnD) / 2))
    where
        tpD = int2Double tp
        fpD = int2Double fp
        fnD = int2Double fn
