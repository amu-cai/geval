module GEval.Common
       where

(/.) :: (Eq a, Integral a) => a -> a -> Double
x /. 0 = 0.0
x /. y = (fromIntegral x) / (fromIntegral y)

safeDoubleDiv :: Double -> Double -> Double
safeDoubleDiv _ 0.0 = 0.0
safeDoubleDiv x y = x / y
