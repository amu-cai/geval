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

getMatchingFunction :: MatchingSpecification -> Text -> Text -> Double
getMatchingFunction ExactMatch = (\a b -> 1.0)
getMatchingFunction FuzzyMatch = (\a b -> 1.0)
getMatchingFunction (CutLabel smatchSpec)= getMatchingFunction smatchSpec
