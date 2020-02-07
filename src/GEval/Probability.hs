{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module GEval.Probability
       (Probability, getP, isProbability, mkProbability, probabilityOne, probabilityZero,
        Coverable, coveredScore, coveredScoreTotal, disjoint, allDisjoint, CoverableEntityWithProbability,
        EntityWithProbability, getProbability, getProbabilityAsDouble, getBareEntity, matchScore, BareEntity,
        precisionScoreTotal, recallScoreTotal)
       where

newtype Probability = P { getP :: Double }
    deriving (Eq, Ord, Show)

isProbability :: Double -> Bool
isProbability p = 0.0 <= p && p <= 1.0

mkProbability :: Double -> Probability
mkProbability p
  | isProbability p = P p
  | otherwise = error $ show p ++ " is not in [0, 1]"

probabilityOne :: Probability
probabilityOne = mkProbability 1.0

probabilityZero :: Probability
probabilityZero = mkProbability 0.0

class EntityWithProbability e where
  type BareEntity e :: *
  getProbability :: e -> Probability
  getProbability = mkProbability . getProbabilityAsDouble
  getProbabilityAsDouble :: e -> Double
  getProbabilityAsDouble = getP . getProbability
  getBareEntity :: e -> BareEntity e
  matchScore :: BareEntity e -> e -> Double

class Coverable b where
  coveredScore :: b -> b -> Double
  coveredScoreTotal :: [b] -> [b] -> Double
  coveredScoreTotal items otherItems = sum $ [coveredScore b b' | b <- items, b' <- otherItems]
  disjoint :: b -> b -> Bool


allDisjoint :: Coverable b => [b] -> Bool
allDisjoint [] = True
allDisjoint (head:tail) = all (disjoint head) tail && allDisjoint tail

class (EntityWithProbability e, Coverable (BareEntity e)) => CoverableEntityWithProbability e where
  precisionScore :: e -> BareEntity e -> Double
  precisionScore got expected = coveredScore (getBareEntity got) expected
  precisionScoreTotal :: [e] -> [BareEntity e] -> Double
  precisionScoreTotal gots expecteds = coveredScoreTotal (map getBareEntity gots) expecteds
  recallScore :: BareEntity e -> e -> Double
  recallScore expected got = coveredScore expected (getBareEntity got)
  recallScoreTotal :: [BareEntity e] -> [e] -> Double
  recallScoreTotal expecteds gots = coveredScoreTotal expecteds (map getBareEntity gots)
