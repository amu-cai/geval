{-# LANGUAGE PartialTypeSignatures #-}

module GEval.PrecisionRecall(fMeasure, f1Measure, f2Measure, precision, recall,
                             fMeasureOnCounts, f1MeasureOnCounts, f2MeasureOnCounts,
                             precisionAndRecall, precisionAndRecallFromCounts)
       where

import GEval.Common

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.MaxFlow

f2Measure :: (a -> b -> Bool) -> [a] -> [b] -> Double
f2Measure = fMeasure 2.0

f1Measure :: (a -> b -> Bool) -> [a] -> [b] -> Double
f1Measure = fMeasure 1.0

fMeasure :: Double -> (a -> b -> Bool) -> [a] -> [b] -> Double
fMeasure beta matchingFun expected got =
  (1 + betaSquared) * p * r `safeDoubleDiv` (betaSquared * p + r)
  where betaSquared = beta ^ 2
        (p, r) = precisionAndRecall matchingFun expected got

f2MeasureOnCounts :: (Int, Int, Int) -> Double
f2MeasureOnCounts = fMeasureOnCounts 2.0

f1MeasureOnCounts :: (Int, Int, Int) -> Double
f1MeasureOnCounts = fMeasureOnCounts 1.0

fMeasureOnCounts :: Double -> (Int, Int, Int) -> Double
fMeasureOnCounts beta (tp, nbExpected, nbGot) =
  (1 + betaSquared) * p * r `safeDoubleDiv` (betaSquared * p + r)
  where betaSquared = beta ^ 2
        (p, r) = precisionAndRecallFromCounts (tp, nbExpected, nbGot)

precisionAndRecall :: (a -> b -> Bool) -> [a] -> [b] -> (Double, Double)
precisionAndRecall matchFun expected got
  = precisionAndRecallFromCounts (tp, length expected, length got)
    where tp = maxMatch matchFun expected got

precisionAndRecallFromCounts :: (Int, Int, Int) -> (Double, Double)
precisionAndRecallFromCounts (tp, nbExpected, nbGot) =
  (tp /. nbGot, tp /. nbExpected)

precision :: (a -> b -> Bool) -> [a] -> [b] -> Double
precision matchFun expected got = fst $ precisionAndRecall matchFun expected got

recall :: (a -> b -> Bool) -> [a] -> [b] -> Double
recall matchFun expected got = snd $ precisionAndRecall matchFun expected got

-- counting maximum match with maximum bipartite matching
-- (we build an auxiliary graph and do a max-flow on this)
maxMatch :: (a -> b -> Bool) -> [a] -> [b] -> Int
maxMatch matchFun expected got = mf
   where (b, e, g) = buildGraph matchFun expected got
         mf = maxFlow g (fst b) (fst e)


buildGraph :: (a -> b -> Bool) -> [a] -> [b] -> (LNode Int, LNode Int, Gr Int Int)
buildGraph matchFun expected got = (b, e, g)
   where ((b, e), (_, g)) = buildGraph' matchFun expected got
         buildGraph' matchFun expected got =
           run empty $
             do b <- insMapNodeM 0
                e <- insMapNodeM 1
                mapM insMapNodeM [2..1+(length expected)+(length got)]
                insMapEdgesM $ map (\n -> (0, n, 1)) expectedIxs
                insMapEdgesM $ map (\m -> (m, 1, 1)) gotIxs
                insMapEdgesM $ map (\(n,m) -> (n, m, 1))
                             $ filter (\(n, m) -> matchFun (expected !! (n-2)) (got !! (m-2-(length expected))))
                               [(x,y) | x <- expectedIxs, y <- gotIxs]
                return (b,e)
                where expectedIxs = [2..1+(length expected)]
                      gotIxs = [2+(length expected)..1+(length expected)+(length got)]
