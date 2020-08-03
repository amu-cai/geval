{-# LANGUAGE LambdaCase #-}

module GEval.Formatting
       (formatTheResult, formatSimpleResult, formatTheResultWithErrorBounds)
       where

import GEval.Common
import Data.Conduit.Bootstrap
import Text.Printf


formatTheResult :: FormattingOptions -> MetricResult -> String
formatTheResult format (SimpleRun val) = formatSimpleResult format val
formatTheResult format (BootstrapResampling vals) = formatTheResultWithErrorBounds format pointEstimate (Just errorBound)
  where pointEstimate = (upperBound + lowerBound) / 2.0
        errorBound = (upperBound - lowerBound) / 2.0
        (lowerBound, upperBound) = getConfidenceBounds defaultConfidenceLevel vals

formatTheResultWithErrorBounds :: FormattingOptions -> MetricValue -> Maybe MetricValue -> String
formatTheResultWithErrorBounds format pointEstimate Nothing = formatSimpleResult format pointEstimate
formatTheResultWithErrorBounds format pointEstimate (Just errorBound) = (formatSimpleResult formatWithCorrectedPrecision pointEstimate)
                                                                            ++ "±"
                                                                            ++ (formatSimpleResult formatWithCorrectedPrecision errorBound)
    where errorBoundMagnitude = (floor (logBase 10.0 errorBound)) - 1
          formatWithCorrectedPrecision = selectLowerPrecision (max (-errorBoundMagnitude) 0) format

formatSimpleResult :: FormattingOptions -> MetricValue -> String
formatSimpleResult = \case
  FormattingOptions (Just prec) True -> printf "%.*f" (max 0 (prec-2)) . (*100)
  FormattingOptions (Just prec) _    -> printf "%.*f" prec
  _                                  -> show

selectLowerPrecision :: Int -> FormattingOptions -> FormattingOptions
selectLowerPrecision p = \case
  FormattingOptions (Just prec) showAsPercentage -> FormattingOptions (Just $ min prec p) showAsPercentage
  FormattingOptions (Nothing) showAsPercentage   -> FormattingOptions (Just p) showAsPercentage
