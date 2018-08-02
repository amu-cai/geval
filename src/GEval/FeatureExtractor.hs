{-# LANGUAGE OverloadedStrings #-}


module GEval.FeatureExtractor
  (extractUnigramFeatures,
   extractUnigramFeaturesFromTabbed)
  where

import Data.Text
import Data.List
import Data.Monoid ((<>))

extractUnigramFeatures :: Text -> Text -> [Text]
extractUnigramFeatures namespace record = Prelude.map (prefix <>) $ nub $ tokenize record
  where prefix = namespace <> ":"

tokenize :: Text -> [Text]
tokenize t = Data.List.filter (not . Data.Text.null) $ split splitPred t
   where splitPred c = c == ' ' || c == '\t' || c == ':'

extractUnigramFeaturesFromTabbed :: Text -> Text -> [Text]
extractUnigramFeaturesFromTabbed namespace record =
  Data.List.concat
  $ Prelude.map (\(n, t) -> extractUnigramFeatures (namespace <> "<" <> (pack $ show n) <> ">") t)
  $ Prelude.zip [1..] (splitOn "\t" record)
