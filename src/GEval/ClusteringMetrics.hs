module GEval.ClusteringMetrics
       (purity, purityFromConfusionMap, updateConfusionMap,
        normalizedMutualInformation,
        normalizedMutualInformationFromConfusionMatrix,
        updateConfusionMatrix)
       where

import GEval.Common

import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.List

purity :: (Hashable a, Eq a, Hashable b, Eq b) => [(a, b)] -> Double
purity pL = purityFromConfusionMap cM
  where cM = confusionMap pL

purityFromConfusionMap :: (Hashable a, Eq a, Hashable b, Eq b) => M.HashMap b (M.HashMap a Int) -> Double
purityFromConfusionMap cM = numberOfMajorityItems /. numberOfItems
   where numberOfItems = sum $ map fst classCounts
         numberOfMajorityItems = sum $ map snd classCounts
         classCounts = map getClassCount $ M.toList cM
         getClassCount (_, sh) = foldl' (\(s, m) (_, c) -> (s + c, max m c)) (0, 0) $ M.toList sh

confusionMap :: (Hashable a, Eq a, Hashable b, Eq b) => [(a, b)] -> M.HashMap b (M.HashMap a Int)
confusionMap = foldl' updateConfusionMap M.empty

updateConfusionMap :: (Hashable a, Eq a, Hashable b, Eq b) => M.HashMap b (M.HashMap a Int) -> (a, b) -> M.HashMap b (M.HashMap a Int)
updateConfusionMap h (e, g) = M.insertWith updateSubHash g (unitHash e) h
  where  unitHash k = M.singleton k 1
         updateSubHash uh sh = M.unionWith (+) uh sh


normalizedMutualInformation :: (Hashable a, Eq a, Hashable b, Eq b) => [(a, b)] -> Double
normalizedMutualInformation pL = normalizedMutualInformationFromConfusionMatrix cM
  where cM = confusionMatrix pL


normalizedMutualInformationFromConfusionMatrix :: (Hashable a, Eq a, Hashable b, Eq b) => M.HashMap (a, b) Int -> Double
normalizedMutualInformationFromConfusionMatrix cM = 2.0 * mutualInformation / (classEntropy + clusterEntropy)
  where mutualInformation = sum $ map pairMutualInformation $ M.toList cM
        pairMutualInformation ((klass, cluster), count) =
          (count /. total) * (log2 ((total /. (classDistribution M.! klass)) * (count /. (clusterDistribution M.! cluster))))
        total = sum $ map snd $ M.toList cM

        classEntropy = entropyWithTotalGiven total $ map snd $ M.toList classDistribution
        clusterEntropy = entropyWithTotalGiven total $ map snd $ M.toList clusterDistribution

        classDistribution = getDistribution fst cM
        clusterDistribution = getDistribution snd cM

        getDistribution fun cM = M.foldlWithKey' (\m kv count -> M.insertWith (+) (fun kv) count m) M.empty cM


confusionMatrix :: (Hashable a, Eq a, Hashable b, Eq b) => [(a, b)] -> M.HashMap (a, b) Int
confusionMatrix = foldl' updateConfusionMatrix M.empty

updateConfusionMatrix :: (Hashable a, Eq a, Hashable b, Eq b) => M.HashMap (a, b) Int -> (a, b) -> M.HashMap (a, b) Int
updateConfusionMatrix m p = M.insertWith (+) p 1 m
