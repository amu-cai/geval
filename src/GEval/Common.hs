module GEval.Common
       where

(/.) :: (Eq a, Integral a) => a -> a -> Double
x /. 0 = 1.0
x /. y = (fromIntegral x) / (fromIntegral y)

safeDoubleDiv :: Double -> Double -> Double
safeDoubleDiv _ 0.0 = 0.0
safeDoubleDiv x y = x / y

log2 :: Double -> Double
log2 x = (log x) / (log 2.0)

entropyWithTotalGiven total distribution = - (sum $ map (entropyCount total) distribution)

entropyCount :: Int -> Int -> Double
entropyCount total count = prob * (log2 prob)
  where prob = count /. total
