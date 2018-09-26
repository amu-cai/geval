module GEval.WER
       (werStep)
       where

import Data.Array
import GEval.Common

werStep :: Eq a => [a] -> [a] -> Double
werStep expected got = (fromIntegral $ distance expected got) `safeDoubleDiv` (fromIntegral $ length expected)

-- see https://stackoverflow.com/questions/6718787/levenshtein-distance-cost
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