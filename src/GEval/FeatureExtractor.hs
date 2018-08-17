{-# LANGUAGE OverloadedStrings #-}


module GEval.FeatureExtractor
  (extractUnigramFeatures,
   extractUnigramFeaturesFromTabbed)
  where

import Data.Text
import Data.List
import Data.Monoid ((<>))
import Text.Tokenizer

extractUnigramFeatures :: (Maybe Tokenizer) -> Text -> Text -> [Text]
extractUnigramFeatures mTokenizer namespace record = Prelude.map (prefix <>) $ nub $ (tokenizeForFeatures mTokenizer) record
  where prefix = namespace <> ":"

tokenizeForFeatures :: (Maybe Tokenizer) -> Text -> [Text]
tokenizeForFeatures Nothing t = Data.List.filter (not . Data.Text.null) $ split splitPred t
   where splitPred c = c == ' ' || c == '\t' || c == ':'
tokenizeForFeatures mTokenizer t = tokenize mTokenizer t

extractUnigramFeaturesFromTabbed :: (Maybe Tokenizer) -> Text -> Text -> [Text]
extractUnigramFeaturesFromTabbed mTokenizer namespace record =
  Data.List.concat
  $ Prelude.map (\(n, t) -> extractUnigramFeatures mTokenizer (namespace <> "<" <> (pack $ show n) <> ">") t)
  $ Prelude.zip [1..] (splitOn "\t" record)
