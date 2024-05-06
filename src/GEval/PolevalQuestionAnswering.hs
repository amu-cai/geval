module GEval.PolevalQuestionAnswering
    () where


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


getNLevenshteinSingleLine :: String -> String -> (Bolean, Double)
getNLevenshteinSingleLine expected output
    | expected == "" = (False, 0.0)
    | otherwise = (True, metricValue)
    where
        maxLength = maximum [length expected, length output]
        metricvalue = 1 - ((distance expected output) / maxLength)


distance :: Eq e => [e] -> [e] -> Int
distance u v = memo ! (m, n)
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


polevalQACond :: Monad m => ConduitM (Bolean, Double) o m AggregatedResult
polevalQACond = Data.Conduit.Combinators.foldl countPolevalAgg aggregatedResultZero


polevalQACount :: AggregatedResult -> (Bolean, Double) -> AggregatedResult
polevalQACount agg (cond, num)
    | cond = AggregatedResult
        { sumAgg = newSum
        , avgAgg = newSum / newNLines
        , nLines = newNLines
        }
    | otherwise = agg
    where
        newSum = sumAgg agg + num
        newNLines = succ $ nLines agg


nLevenshteinPoleval :: AggregatedResult -> Double
nLevenshteinPoleval = avgAgg
