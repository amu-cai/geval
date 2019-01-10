{-# LANGUAGE OverloadedStrings #-}

module GEval.FeatureExtractor
  (extractFeatures,
   extractFeaturesFromTabbed,
   cartesianFeatures,
   LineWithFeatures(..),
   LineWithPeggedFeatures(..),
   PeggedFeature(..),
   Feature(..))
  where

import Data.Text
import Data.List
import Data.Monoid ((<>))
import Text.Tokenizer
import Text.WordShape
import GEval.BlackBoxDebugging
import GEval.Common

data LineWithFeatures = LineWithFeatures Double MetricValue [Feature]
                              deriving (Eq, Ord)

data Feature = UnaryFeature PeggedFeature | CartesianFeature PeggedFeature PeggedFeature
               deriving (Eq, Ord)

instance Show Feature where
  show (UnaryFeature feature) = show feature
  show (CartesianFeature featureA featureB) = (show featureA) ++ "~~" ++ (show featureB)

data LineWithPeggedFeatures = LineWithPeggedFeatures Double MetricValue [PeggedFeature]
                              deriving (Eq, Ord)

data PeggedFeature = PeggedFeature FeatureNamespace SimpleFeature
               deriving (Eq, Ord)

instance Show PeggedFeature where
  show (PeggedFeature namespace feature) = (show namespace) ++ ":" ++ (show feature)

data SimpleFeature = SimpleAtomicFeature AtomicFeature | BigramFeature AtomicFeature AtomicFeature
               deriving (Eq, Ord)

instance Show SimpleFeature where
  show (SimpleAtomicFeature feature) = show feature
  show (BigramFeature featureA featureB) = (show featureA) ++ "++" ++ (show featureB)

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

extractSimpleFeatures :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> [SimpleFeature]
extractSimpleFeatures mTokenizer bbdo t = Data.List.concat $ (Prelude.map (Prelude.map SimpleAtomicFeature) atomss) ++
                                                             if bbdoBigrams bbdo
                                                             then Prelude.map bigramFeatures atomss
                                                             else []
  where atomss = extractAtomicFeatures mTokenizer bbdo t
        bigramFeatures atoms = Prelude.map (\(a, b) -> BigramFeature a b) $ bigrams atoms

extractFeatures :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> Text -> [PeggedFeature]
extractFeatures mTokenizer bbdo namespace record =
  Prelude.map (\af -> PeggedFeature (FeatureNamespace namespace) af)
  $ extractSimpleFeatures mTokenizer bbdo record

extractFeaturesFromTabbed :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> Text -> [PeggedFeature]
extractFeaturesFromTabbed mTokenizer bbdo namespace record =
  Data.List.concat
  $ Prelude.map (\(n, t) -> Prelude.map (\af -> PeggedFeature (FeatureTabbedNamespace namespace n) af) $ extractSimpleFeatures mTokenizer bbdo t)
  $ Prelude.zip [1..] (splitOn "\t" record)

addCartesianFeatures :: BlackBoxDebuggingOptions -> [LineWithPeggedFeatures] -> [LineWithFeatures]
addCartesianFeatures bbdo linesWithPeggedFeatures = addCartesianFeatures' (bbdoCartesian bbdo) linesWithPeggedFeatures
  where addCartesianFeatures' _ linesWithPeggedFeatures
          = Prelude.map (\(LineWithPeggedFeatures rank score fs) ->
                            LineWithFeatures rank score (Prelude.map UnaryFeature fs)) linesWithPeggedFeatures

cartesianFeatures :: [PeggedFeature] -> [Feature]
cartesianFeatures features = nub $ [CartesianFeature a b | a <- features, b <- features, a < b]
