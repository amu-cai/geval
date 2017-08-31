module GEval.CharMatch
       (getCharMatchCount, charMatchBeta)
       where

import Text.EditDistance

charMatchBeta :: Double
charMatchBeta = 1.0

getCharMatchCount :: String -> String -> String -> (Int, Int, Int)
getCharMatchCount input expected output = (correctionsDone, expectedCorrections, distanceToInput)
  where expectedCorrections = ld input expected
        distanceToInput = ld input output
        distanceToExpected = ld output expected
        correctionsDone = min expectedCorrections $
                              (max 0 (expectedCorrections + distanceToInput - distanceToExpected)) `div` 2
        ld a b = levenshteinDistance defaultEditCosts a b
