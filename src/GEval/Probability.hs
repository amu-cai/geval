
module GEval.Probability
       (Probability, getP, isProbability, mkProbability, probabilityOne, probabilityZero)
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
