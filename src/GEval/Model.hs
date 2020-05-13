{-# LANGUAGE OverloadedStrings #-}

module GEval.Model(
  ModelType(..),
  SimpleMixEnsembleModel(..),
  SimpleMixEnsembleModelRule(..))
  where

import qualified Data.ByteString.UTF8 as BSU
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light.Base (Regex(..))

import qualified Data.Vector as V

import Data.Either (fromRight)

import Data.Yaml

data ModelType = SimpleMixEnsemble
  deriving (Show, Read, Eq)

data SimpleMixEnsembleModel = SimpleMixEnsembleModel [SimpleMixEnsembleModelRule]
  deriving (Show)

data SimpleMixEnsembleModelRule = SimpleMixEnsembleModelRule {
  simpleMixEnsembleModelRuleRegex :: Regex,
  simpleMixEnsembleModelRuleOut :: String }
  deriving (Eq, Show)

instance ToJSON SimpleMixEnsembleModelRule where
  toJSON rule = object ["regex" .= (formatRegexp $ simpleMixEnsembleModelRuleRegex rule),
                        "out" .= simpleMixEnsembleModelRuleOut rule]

instance FromJSON SimpleMixEnsembleModelRule where
  parseJSON = withObject "SimpleMixEnsembleModelRule" $ \v -> SimpleMixEnsembleModelRule
        <$> (toRegexp <$> (v .: "regex"))
        <*> v .: "out"

instance FromJSON SimpleMixEnsembleModel where
  parseJSON = withArray "SimpleMixEnsembleModel" $ \arr ->
               SimpleMixEnsembleModel <$> mapM parseJSON (V.toList arr)

instance ToJSON SimpleMixEnsembleModel where
  toJSON (SimpleMixEnsembleModel rules) = toJSON rules

formatRegexp (Regex _ regexp) = BSU.toString regexp

toRegexp = (fromRight undefined) . ((flip compileM) []) . BSU.fromString
