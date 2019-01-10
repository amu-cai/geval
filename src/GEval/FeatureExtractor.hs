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
import Text.WordShape
import GEval.BlackBoxDebugging

data Feature = SimpleFeature FeatureNamespace AtomicFeature
               deriving (Eq, Ord)

instance Show Feature where
  show (SimpleFeature namespace feature) = (show namespace) ++ ":" ++ (show feature)

data AtomicFeature = TextFeature Text | ShapeFeature WordShape
                     deriving (Eq, Ord)

instance Show AtomicFeature where
  show (TextFeature t) = unpack t
  show (ShapeFeature (WordShape t)) = 'S':'H':'A':'P':'E':':':(unpack t)

data FeatureNamespace = FeatureNamespace Text | FeatureTabbedNamespace Text Int
                        deriving (Eq, Ord)

instance Show FeatureNamespace where
  show (FeatureNamespace namespace) = unpack namespace
  show (FeatureTabbedNamespace namespace column) = ((unpack namespace) ++ "<" ++ (show column) ++ ">")

tokenizeForFeatures :: (Maybe Tokenizer) -> Text -> [Text]
tokenizeForFeatures Nothing t = Data.List.filter (not . Data.Text.null) $ split splitPred t
   where splitPred c = c == ' ' || c == '\t' || c == ':'
tokenizeForFeatures mTokenizer t = tokenize mTokenizer t

extractAtomicFeatures :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> [[AtomicFeature]]
extractAtomicFeatures mTokenizer bbdo t = [Data.List.map TextFeature tokens] ++
  (if bbdoWordShapes bbdo
    then [nub $ Data.List.map (ShapeFeature . shapify) tokens]
    else [])
  where tokens = nub $ (tokenizeForFeatures mTokenizer) t


extractUnigramFeatures :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> Text -> [Feature]
extractUnigramFeatures mTokenizer bbdo namespace record =
  Prelude.map (\af -> SimpleFeature (FeatureNamespace namespace) af)
  $ Data.List.concat
  $ extractAtomicFeatures mTokenizer bbdo record

extractUnigramFeaturesFromTabbed :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> Text -> [Feature]
extractUnigramFeaturesFromTabbed mTokenizer bbdo namespace record =
  Data.List.concat
  $ Prelude.map (\(n, t) -> Prelude.map (\af -> SimpleFeature (FeatureTabbedNamespace namespace n) af) $ Data.List.concat $ extractAtomicFeatures mTokenizer bbdo t)
  $ Prelude.zip [1..] (splitOn "\t" record)
