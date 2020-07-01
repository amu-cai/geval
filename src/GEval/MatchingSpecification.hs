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

-- | The data type for storing a matching specification
singletons [d|data MatchingSpecification = ExactMatch -- ^ exact match, i.e. identity is required
                                         | FuzzyMatch -- ^ fuzzy match by Levenshtein distance
                                         | CutLabel MatchingSpecification -- ^ require that the label (part before up to `=`)
                                                                          -- is matched and then proceed with some matching spec.
                           deriving (Eq)
             |]

getMatchingFunctionForString :: MatchingSpecification -> String -> String -> Double
getMatchingFunctionForString ExactMatch a b
  | a == b = 1.0
  | otherwise = 0.0
getMatchingFunctionForString FuzzyMatch a b = 1.0
getMatchingFunctionForString (CutLabel smatchSpec) a b = getMatchingFunctionForString smatchSpec a b

getMatchingFunctionForText :: MatchingSpecification -> Text -> Text -> Double
getMatchingFunctionForText matchSpec a b = getMatchingFunctionForString matchSpec (unpack a) (unpack b)
