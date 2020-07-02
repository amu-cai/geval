{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module is for defining possible "matching specifications",
-- i.e. the way tokens are matched against one another in metrics such as MultiLabel-F1.
--
-- Not all metrics could be affected by matching specifications (e.g. they would
-- not make sense for metrics comparing numbers, such as MSE or MAE).

module GEval.MatchingSpecification
  where

import Data.Singletons.TH
import Data.Text
import Data.List.Extra (breakOn)

import Text.EditDistance

-- | The data type for storing a matching specification
singletons [d|data MatchingSpecification = ExactMatch -- ^ exact match, i.e. identity is required
                                         | FuzzyMatch -- ^ fuzzy match by Levenshtein distance
                                         | CutLabel MatchingSpecification -- ^ require that the label (part before up to `=`)
                                                                          -- is matched and then proceed with some matching spec.
                           deriving (Eq)
             |]

getMatchingFunctionForString :: MatchingSpecification -> String -> String -> Double
getMatchingFunctionForString ExactMatch got expected
  | got == expected = 1.0
  | otherwise = 0.0
getMatchingFunctionForString FuzzyMatch got expected = max 0.0 (1.0 - charError)
  where charError = (fromIntegral editDist) / (fromIntegral $ Prelude.length expected)
        editDist = levenshteinDistance defaultEditCosts got expected

getMatchingFunctionForString (CutLabel smatchSpec) a b = getMatchingFunctionForString smatchSpec a' b'
  where a' = cutLabel a
        b' = cutLabel b

-- | Remove the label along with the separator (the equal sign)
cutLabel :: String -> String
cutLabel t = case Data.List.Extra.breakOn "=" t of
  (t, "") -> t -- no label
  (_, valWithSeparator) -> Prelude.tail valWithSeparator

getMatchingFunctionForText :: MatchingSpecification -> Text -> Text -> Double
getMatchingFunctionForText matchSpec a b = getMatchingFunctionForString matchSpec (unpack a) (unpack b)
