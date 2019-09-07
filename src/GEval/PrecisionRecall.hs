{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GEval.PrecisionRecall(calculateMAPForOneResult,
                             weightedHarmonicMean, fMeasure, f1Measure, f2Measure, precision, recall,
                             fMeasureOnCounts, f1MeasureOnCounts, f2MeasureOnCounts, countFolder,
                             precisionAndRecall, precisionAndRecallFromCounts,
                             maxMatch, maxMatchOnOrdered, getCounts, weightedMaxMatch, weightedMaxMatching,
                             getProbabilisticCounts)
       where

import GEval.Common
import GEval.Probability

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.MaxFlow

import Data.List (find, foldl', nub)

import Data.Algorithm.Munkres
import qualified Data.Array.IArray as DAI


calculateMAPForOneResult :: (Eq a) => [a] -> [a] -> Double
calculateMAPForOneResult expected got = precisionSum / fromIntegral (length expected)
  where (_::Int, _, precisionSum) = calculateMAPForOneResultCore expected (nub got)
        calculateMAPForOneResultCore expected got = foldl' (oneMAPStep expected) (0, 0, 0.0) got
        oneMAPStep expected (gotCount, allCount, precisionSum) gotItem
          | gotItem `elem` expected = (newGotCount, newAllCount, precisionSum + (newGotCount /. newAllCount))
          | otherwise = (gotCount, newAllCount, precisionSum)
         where newGotCount = gotCount + 1
               newAllCount = allCount + 1

f2Measure :: (a -> b -> Bool) -> [a] -> [b] -> Double
f2Measure = fMeasure 2.0

f1Measure :: (a -> b -> Bool) -> [a] -> [b] -> Double
f1Measure = fMeasure 1.0

-- | Calculates both generalized) F-measure
fMeasure :: Double          -- ^ beta parameter
         -> (a -> b -> Bool)  -- ^ function to check whether there is a match
         -> [a]             -- ^ the ground truth
         -> [b]             -- ^ what we got
         -> Double          -- ^ f-Measure
fMeasure beta matchingFun expected got = weightedHarmonicMean beta p r
  where (p, r) = precisionAndRecall matchingFun expected got

weightedHarmonicMean :: Double -> Double -> Double -> Double
weightedHarmonicMean beta x y =
  (1 + betaSquared) * x * y `safeDoubleDiv` (betaSquared * x + y)
  where betaSquared = beta ^ 2

f2MeasureOnCounts :: ConvertibleToDouble n => (n, Int, Int) -> Double
f2MeasureOnCounts = fMeasureOnCounts 2.0

f1MeasureOnCounts :: ConvertibleToDouble n => (n, Int, Int) -> Double
f1MeasureOnCounts = fMeasureOnCounts 1.0

fMeasureOnCounts :: (ConvertibleToDouble n, Integral v) => Double -> (n, v, v) -> Double
fMeasureOnCounts beta (tp, nbExpected, nbGot) =
  (1 + betaSquared) * p * r `safeDoubleDiv` (betaSquared * p + r)
  where betaSquared = beta ^ 2
        (p, r) = precisionAndRecallFromCounts (tp, nbExpected, nbGot)

countFolder :: (Num n, Num v) => (n, v, v) -> (n, v, v) -> (n, v, v)
countFolder (a1, a2, a3) (b1, b2, b3) = (a1+b1, a2+b2, a3+b3)

getCounts :: (a -> b -> Bool) -> ([a], [b]) -> (Int, Int, Int)
getCounts matchingFun (expected, got) = (maxMatch matchingFun expected got,
                                         length expected,
                                         length got)

-- | Calculates both precision and recall.
--
-- (See https://en.wikipedia.org/wiki/Precision_and_recall)
precisionAndRecall :: (a -> b -> Bool) -- ^ matching function (whether you've got a success)
                   -> [a]            -- ^ ground truth
                   -> [b]            -- ^ what was returned by the system
                   -> (Double, Double) -- ^ returns precision and recall
precisionAndRecall matchFun expected got
  = precisionAndRecallFromCounts (tp, length expected, length got)
    where tp = maxMatch matchFun expected got

precisionAndRecallFromCounts :: (ConvertibleToDouble n, Integral v) => (n, v, v) -> (Double, Double)
precisionAndRecallFromCounts (tp, nbExpected, nbGot) =
  (tp /. nbGot, tp /. nbExpected)

precision :: (a -> b -> Bool) -> [a] -> [b] -> Double
precision matchFun expected got = fst $ precisionAndRecall matchFun expected got

recall :: (a -> b -> Bool) -> [a] -> [b] -> Double
recall matchFun expected got = snd $ precisionAndRecall matchFun expected got


maxMatchOnOrdered :: Eq a => (a -> a -> Bool) -> [a] -> [a] -> Int
maxMatchOnOrdered laterThan expected got =
   let (matched, _) = foldl' step (0, expected) got
   in matched
         where step (matched, l@(h:t)) g
                | h == g = (matched+1, t)
                | h `laterThan` g  = (matched, l)
                | otherwise = step (matched, t) g
               step (matched, []) g = (matched, [])

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

-- the weight are assumed to be between 0.0 and 1.0
weightedMaxMatch :: (a -> b -> Double) -> [a] -> [b] -> Double
weightedMaxMatch matchFun expected got = (fromIntegral $ length matching) - score
   where (matching, score) = weightedMaxMatching matchFun expected got

weightedMaxMatching :: (a -> b -> Double) -> [a] -> [b] -> ([(Int, Int)], Double)
weightedMaxMatching matchFun expected got = hungarianMethodDouble complementWeightArray
                                            -- unfortunately `hungarianMethodDouble` looks
                                            -- for minimal bipartite matching
                                            -- rather than the maximal one
  where  complementWeightArray = DAI.array ((1, 1), (m, n)) weightList
         m = length expected
         n = length got
         weightList = [((i, j), 1.0 - (matchFun x y)) | (i, x) <- zip [1..m] expected,
                                                        (j, y) <- zip [1..n] got]



getProbabilisticCounts :: EntityWithProbability e => ([BareEntity e], [e]) -> ([Double], [Double], Double, Int)
getProbabilisticCounts (expected, got) = (results, (map getProbabilityAsDouble got),
                                              gotMass,
                                              length expected)
  where gotMass = sum $ map (\(i, j) -> (matchScore (expected !! (i - 1)) (got !! (j - 1))) * (getProbabilityAsDouble (got !! (j - 1))))  matching
        results = map findResult [1..(length got)]
        findResult j = case find (\(i, j') -> j' == j) $ matching of
          Just (i, _) -> matchScore (expected !! (i - 1)) (got !! (j - 1))
          Nothing -> 0.0
        (matching, _) = weightedMaxMatching matchScore expected got
