module Data.Statistics.Calibration
   (calibration, softCalibration) where

import Data.Statistics.Loess (clippedLoess)
import Numeric.Integration.TanhSinh
import Data.List (minimum, maximum)
import qualified Data.Vector.Unboxed as DVU

minBand :: Double
minBand = 0.001

bool2Double :: Bool -> Double
bool2Double True = 1.0
bool2Double False = 0.0

mean :: [Double] -> Double
mean results = (sum results) / (fromIntegral n)
  where n = length results

band :: [Double] -> Double
band xs = (maximum xs) - (minimum xs)

calibration :: [Bool] -> [Double] -> Double
calibration results probs = softCalibration results' probs
  where results' = map bool2Double results

integrate :: (Double, Double) -> (Double -> Double) -> Double
integrate (a, b) fun = case simpson fun a b of
  (r:_) -> result r

softCalibration :: [Double] -> [Double] -> Double
softCalibration [] [] = 1.0
softCalibration [] _ = error "too few booleans in calibration"
softCalibration _ [] = error "too few probabilities in calibration"
softCalibration results probs
  | band probs < minBand = handleNarrowBand results probs
  | otherwise = 1.0 - (min 1.0 (2.0 * (integrate (lowest, highest) (\x -> abs ((clippedLoess (DVU.fromList probs) (DVU.fromList results) x) - x))) / (highest - lowest)))
  where lowest = (minimum probs) + epsilon -- integrating loess gets crazy at edges
        highest = (maximum probs) - epsilon
        epsilon = 0.0001

handleNarrowBand :: [Double] -> [Double] -> Double
handleNarrowBand results probs = 1.0 - deviation
  where deviation = abs (g - t)
        g = mean probs
        t = mean results
