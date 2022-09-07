module GEval.WER
       (werStep, errorRate)
       where

import Data.Array

werStep :: Eq a => [a] -> [a] -> (Int, Int)
werStep expected got = (distance expected got, length expected)

errorRate :: Int -> Int -> Double
errorRate 0 0 = 0.0
errorRate _ 0 = 1.0
errorRate got expected = (fromIntegral got) / (fromIntegral expected)

-- see https://stackoverflow.com/questions/6718787/levenshtein-distance-cost
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
