module GEval.PolevalQuestionAnswering
    () where


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
