module GEval.Purity
       (purity, purityFromConfusionMap, updateConfusionMap)
       where

import GEval.Common

import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.List

purity :: (Hashable a, Eq a, Hashable b, Eq b) => [(a, b)] -> Double
purity pL = purityFromConfusionMap cM
  where cM = confusionMap pL

purityFromConfusionMap :: (Hashable a, Eq a, Hashable b, Eq b) => M.HashMap a (M.HashMap b Int) -> Double
purityFromConfusionMap cM = numberOfMajorityItems /. numberOfItems
   where numberOfItems = sum $ map fst classCounts
         numberOfMajorityItems = sum $ map snd classCounts
         classCounts = map getClassCount $ M.toList cM
         getClassCount (_, sh) = foldl' (\(s, m) (_, c) -> (s + c, max m c)) (0, 0) $ M.toList sh

confusionMap :: (Hashable a, Eq a, Hashable b, Eq b) => [(a, b)] -> M.HashMap a (M.HashMap b Int)
confusionMap = foldl' updateConfusionMap M.empty

updateConfusionMap :: (Hashable a, Eq a, Hashable b, Eq b) => M.HashMap a (M.HashMap b Int) -> (a, b) -> M.HashMap a (M.HashMap b Int)
updateConfusionMap h (e, g) = M.insertWith updateSubHash e (unitHash g) h
  where  unitHash k = M.singleton k 1
         updateSubHash uh sh = M.unionWith (+) uh sh
