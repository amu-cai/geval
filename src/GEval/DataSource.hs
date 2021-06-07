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

import qualified Data.Set as S

data Filter = NoFilter | FilterByFeatures (Maybe TabularHeader) (S.Set String)

noFilter :: Filter
noFilter = NoFilter

applyFilter :: Filter -> TargetRecord -> Bool
applyFilter NoFilter _ = True
applyFilter (FilterByFeatures mInHeader featureSpec) tR = applyFeatureFilter mInHeader featureSpec tR

getFilterForScheme :: Maybe TabularHeader -> EvaluationScheme -> Filter
getFilterForScheme mTabHeader (EvaluationScheme _ ops) = case findFilter ops of
  [] -> NoFilter
  fs -> FilterByFeatures mTabHeader (S.fromList $ Prelude.map (unpack . fixIndex) fs)

fixIndex = replace "[" "<" . replace "]" ">"

findFilter :: [PreprocessingOperation] -> [Text]
findFilter [] = []
findFilter ((FeatureFilter f):ops) = (f:(findFilter ops))
findFilter (_:ops) = findFilter ops


applyFeatureFilter :: Maybe TabularHeader -> S.Set String -> TargetRecord -> Bool
applyFeatureFilter mInHeader featureSpec tR = featureSpec `S.isSubsetOf` (S.fromList $ Prelude.map show fs)
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
  challengeDataSourceOutPreprocess :: Text -> Text,
  challengeDataSourceInPreprocess :: Text -> Text,
  challengeDataSourceFilter :: Filter,
  challengeDataSourceInHeader :: Maybe TabularHeader,
  challengeDataSourceOutHeader :: Maybe TabularHeader,
  -- whether the data will be shown preprocessed (not only
  -- the evaluation will be done on the preprocessed data)
  challengeDataSourceShowPreprocessed :: Bool }

-- | This type specifies all the data flowing into evaluation,
-- including the output data to be evaluated.
data DataSource = DataSource {
  dataSourceChallengeData :: ChallengeDataSource,
  dataSourceOut :: SourceSpec }
