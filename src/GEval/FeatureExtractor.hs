{-# LANGUAGE OverloadedStrings #-}


module GEval.FeatureExtractor
  (extractUnigramFeatures,
   extractUnigramFeaturesFromTabbed,
   Feature(..))
  where

import Data.Text
import Data.List
import Data.Monoid ((<>))
import Text.Tokenizer

data Feature = SimpleFeature FeatureNamespace AtomicFeature
               deriving (Eq, Ord)

instance Show Feature where
  show (SimpleFeature namespace feature) = (show namespace) ++ ":" ++ (show feature)

data AtomicFeature = TextFeature Text
                     deriving (Eq, Ord)

instance Show AtomicFeature where
  show (TextFeature t) = unpack t

data FeatureNamespace = FeatureNamespace Text | FeatureTabbedNamespace Text Int
                        deriving (Eq, Ord)

instance Show FeatureNamespace where
  show (FeatureNamespace namespace) = unpack namespace
  show (FeatureTabbedNamespace namespace column) = ((unpack namespace) ++ "<" ++ (show column) ++ ">")

tokenizeForFeatures :: (Maybe Tokenizer) -> Text -> [Text]
tokenizeForFeatures Nothing t = Data.List.filter (not . Data.Text.null) $ split splitPred t
   where splitPred c = c == ' ' || c == '\t' || c == ':'
tokenizeForFeatures mTokenizer t = tokenize mTokenizer t

extractAtomicFeatures :: (Maybe Tokenizer) -> Text -> [AtomicFeature]
extractAtomicFeatures mTokenizer = nub . (Data.List.map TextFeature) . (tokenizeForFeatures mTokenizer)

extractUnigramFeatures :: (Maybe Tokenizer) -> Text -> Text -> [Feature]
extractUnigramFeatures mTokenizer namespace record =
  Prelude.map (\af -> SimpleFeature (FeatureNamespace namespace) af)
  $ extractAtomicFeatures mTokenizer record

extractUnigramFeaturesFromTabbed :: (Maybe Tokenizer) -> Text -> Text -> [Feature]
extractUnigramFeaturesFromTabbed mTokenizer namespace record =
  Data.List.concat
  $ Prelude.map (\(n, t) -> Prelude.map (\af -> SimpleFeature (FeatureTabbedNamespace namespace n) af) $ extractAtomicFeatures mTokenizer t)
  $ Prelude.zip [1..] (splitOn "\t" record)
