{-# LANGUAGE OverloadedStrings #-}

module GEval.DataSource
  (ChallengeDataSource(..),
   DataSource(..),
   TargetRecord(..),
   Filter(..),
   noFilter,
   getFilterForScheme,
   applyFilter)
  where

import GEval.Common (SourceItem(..))
import GEval.Selector (ItemTarget(..), TargetRecord(..))
import GEval.FeatureExtractor (getFeatures)
import GEval.BlackBoxDebugging
import GEval.EvaluationScheme

import Data.Text

import Data.Conduit.SmartSource
import Data.Conduit.Header
import GEval.Selector

data Filter = NoFilter | FilterByFeatures (Maybe TabularHeader) String

noFilter :: Filter
noFilter = NoFilter

applyFilter :: Filter -> TargetRecord -> Bool
applyFilter NoFilter _ = True
applyFilter (FilterByFeatures mInHeader featureSpec) tR = applyFeatureFilter mInHeader featureSpec tR

getFilterForScheme :: Maybe TabularHeader -> EvaluationScheme -> Filter
getFilterForScheme mTabHeader (EvaluationScheme _ ops) = case findFilter ops of
  Nothing -> NoFilter
  Just f -> FilterByFeatures mTabHeader (unpack $ fixIndex f)

fixIndex = replace "[" "<" . replace "]" ">"

findFilter :: [PreprocessingOperation] -> Maybe Text
findFilter [] = Nothing
findFilter ((FeatureFilter f):_) = Just f
findFilter (_:ops) = findFilter ops


applyFeatureFilter :: Maybe TabularHeader -> String -> TargetRecord -> Bool
applyFeatureFilter mInHeader featureSpec tR = featureSpec `elem` (Prelude.map show fs)
  where fs = getFeatures Nothing
                         BlackBoxDebuggingOptions {
                           bbdoMinFrequency = 0,
                           bbdoWordShapes = False,
                           bbdoBigrams = False,
                           bbdoCartesian = False,
                           bbdoMinCartesianFrequency = Nothing,
                           bbdoConsiderNumericalFeatures = False}
                         Nothing
                         tR
                         mInHeader


-- | This type specifies the way the challenge data (input and
-- expected data, but not outputs) flow into evaluation.
--
-- At some point, it is turned into conduit for reading data.
data ChallengeDataSource = ChallengeDataSource {
  challengeDataSourceInput :: SourceSpec,
  challengeDataSourceExpected :: SourceSpec,
  challengeDataSourceSelector :: Maybe Selector,
  challengeDataSourcePreprocess :: Text -> Text,
  challengeDataSourceFilter :: Filter,
  challengeDataSourceInHeader :: Maybe TabularHeader,
  challengeDataSourceOutHeader :: Maybe TabularHeader }

-- | This type specifies all the data flowing into evaluation,
-- including the output data to be evaluated.
data DataSource = DataSource {
  dataSourceChallengeData :: ChallengeDataSource,
  dataSourceOut :: SourceSpec }
