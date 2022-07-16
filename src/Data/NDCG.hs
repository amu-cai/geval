
module Data.NDCG
  (ndcgAt)
where

import Data.Map as M hiding (map, take)
import Data.List

ndcgAt :: Ord a => Int -> M.Map a Double -> [a] -> Double
ndcgAt n relevanceScores ranking = dcgAt n relevanceScores ranking / idcgAt n relevanceScores

dcgAt :: Ord a => Int -> M.Map a Double -> [a] -> Double
dcgAt n relevanceScores ranking = dcg relevanceScores (take n ranking)

dcg :: Ord a => M.Map a Double -> [a] -> Double
dcg relevanceScores ranking =
  dcgForRanks $ map (\k -> findWithDefault 0.0 k relevanceScores) ranking

dcgForRanks :: [Double] -> Double
dcgForRanks scores =
  sum
  $ map (\(i, r) -> r / (logBase 2.0 (i + 1.0)))
  $ zip [1..] scores

idcgAt :: Ord a => Int -> M.Map a Double -> Double
idcgAt n relevanceScores = dcgForRanks $ take n $ sortBy (flip compare) $ elems relevanceScores
