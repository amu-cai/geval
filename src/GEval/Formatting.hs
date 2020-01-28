module GEval.Formatting
       (formatTheResult, formatSimpleResult)
       where

import GEval.Common
import Data.Conduit.Bootstrap
import Text.Printf


formatTheResult :: Maybe Int -> MetricResult -> String
formatTheResult mPrecision (SimpleRun val) = formatSimpleResult mPrecision val
formatTheResult mPrecision (BootstrapResampling vals) = (formatSimpleResult correctedPrecision pointEstimate)
                                                        ++ "±"
                                                        ++ (formatSimpleResult correctedPrecision errorBound)
    where pointEstimate = (upperBound + lowerBound) / 2.0
          errorBound = (upperBound - lowerBound) / 2.0
          (lowerBound, upperBound) = getConfidenceBounds defaultConfidenceLevel vals
          errorBoundMagnitude = (floor (logBase 10.0 errorBound)) - 1
          correctedPrecision = Just $ selectLowerPrecision (max (-errorBoundMagnitude) 0) mPrecision

formatSimpleResult :: Maybe Int -> MetricValue -> String
formatSimpleResult Nothing = show
formatSimpleResult (Just prec) = printf "%0.*f" prec

selectLowerPrecision :: Int -> Maybe Int -> Int
selectLowerPrecision p Nothing = p
selectLowerPrecision p (Just p') = min p p'
