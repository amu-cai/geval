{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

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
import Data.Char (isLetter, toLower)
import Data.List (find)
import Data.Maybe (isJust, listToMaybe, fromJust, catMaybes, fromMaybe)
import Text.Regex.PCRE.Heavy

import Text.EditDistance

-- | The data type for storing a matching specification
singletons [d|data MatchingSpecification = ExactMatch -- ^ exact match, i.e. identity is required
                                         | FuzzyMatch -- ^ fuzzy match by Levenshtein distance
                                         | CutLabel MatchingSpecification -- ^ require that the label (part before up to `=`)
                                                                          -- is matched and then proceed with some matching spec.
                                         | SmartMatch MatchingSpecification -- ^ do fuzzy matching only on values
                                                                            -- containing letters
                                         | Harden MatchingSpecification -- ^ harden a soft match
                                         | LenientHarden MatchingSpecification -- ^ harden a soft match (lenient variant)
                                         | Lower MatchingSpecification -- ^ lower-case inputs
                                         | ExtractNumber MatchingSpecification -- ^ try extracting numbers first
                                         deriving (Eq)
             |]

hardeningThreshold :: Double
hardeningThreshold = 0.8

lenientHardeningThreshold :: Double
lenientHardeningThreshold = 0.5

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

getMatchingFunctionForString (SmartMatch smatchSpec) got expected = getMatchingFunctionForString chosenMatch got expected
  where chosenMatch = if wantedBySmartMatch expected
                      then smatchSpec
                      else ExactMatch

getMatchingFunctionForString (Harden smatchSpec) got expected = if softMatch >= hardeningThreshold
                                                                then 1.0
                                                                else 0.0
  where softMatch = getMatchingFunctionForString smatchSpec got expected

getMatchingFunctionForString (LenientHarden smatchSpec) got expected = if softMatch >= lenientHardeningThreshold
                                                                       then 1.0
                                                                       else 0.0
  where softMatch = getMatchingFunctionForString smatchSpec got expected

getMatchingFunctionForString (Lower smatchSpec) got expected =
  getMatchingFunctionForString smatchSpec (lowerS got)
                                          (lowerS expected)
  where lowerS = Prelude.map Data.Char.toLower

getMatchingFunctionForString (ExtractNumber smatchSpec) got expected =
  if isJust en
  then
    if gn == en
    then 1.0
    else 0.0
  else m
  where m = getMatchingFunctionForString smatchSpec got expected
        gn = extractNumber got
        en = extractNumber expected

extractNumber :: String -> Maybe String
extractNumber s = case extractArabicNumber s of
                    Just n -> Just n
                    Nothing -> fmap show (listToMaybe $ catMaybes $ Prelude.map extractRomanNumber $ Prelude.words s)

extractArabicNumber :: String -> Maybe String
extractArabicNumber s = fst <$> listToMaybe (scan [re|[-+]?\d*\.\d+|\d+|] s)

extractRomanNumber s = case romanToInt s of
                         Just v -> if v > 0
                                  then Just v
                                  else Nothing
                         Nothing -> Nothing

-- see https://wiki.haskell.org/Roman_numerals
romanToInt :: String -> Maybe Int
romanToInt = fst . Prelude.foldr step (Just 0, 0)
                 . Prelude.map (flip lookup (Prelude.zip "IVXLCDM" [1, 5, 10, 50, 100, 500, 1000]))
  where step :: Maybe Int -> (Maybe Int, Int) -> (Maybe Int, Int)
        step _ (Nothing, p) = (Nothing, p)
        step Nothing (_, p) = (Nothing, p)
        step (Just p) (Just t, s) = if p >= s then (Just (t+p), p) else (Just (t-p), p)

-- | Whether suitable for fuzzy matching when in the "smart" match mode.
-- At the moment we check whether it contains at least one letter
-- (we require the exact match for, for instance, numbers written with digits.
wantedBySmartMatch = isJust . (Data.List.find isLetter)

-- | Remove the label along with the separator (the equal sign)
cutLabel :: String -> String
cutLabel t = case Data.List.Extra.breakOn "=" t of
  (t, "") -> t -- no label
  (_, valWithSeparator) -> Prelude.tail valWithSeparator

getMatchingFunctionForText :: MatchingSpecification -> Text -> Text -> Double
getMatchingFunctionForText matchSpec a b = getMatchingFunctionForString matchSpec (unpack a) (unpack b)
