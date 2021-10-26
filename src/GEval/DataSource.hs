{-# LANGUAGE OverloadedStrings #-}

module GEval.DataSource
  (ChallengeDataSource(..),
   DataSource(..),
   TargetRecord(..),
   Filter(..),
   Topper(..),
   GeneralizedFilter(..),
   noFilter,
   hasNoFilter,
   getGeneralizedFilterForScheme,
   applyFilter)
  where

import GEval.Common (SourceItem(..))
import GEval.Selector (ItemTarget(..), TargetRecord(..))
import GEval.FeatureExtractor (getFeatures)
import GEval.BlackBoxDebugging
import GEval.EvaluationScheme
import GEval.Confidence (totalLineConfidence)

import Data.Text

import Data.Conduit.SmartSource
import Data.Conduit.Header
import GEval.Selector

import qualified Data.Set as S

data Filter = NoFilter | FilterByFeatures (Maybe TabularHeader) (S.Set String)
data Topper = NoTopper | Topper Double (TargetRecord -> Double)

data GeneralizedFilter = GeneralizedFilter {
  generalizedFilterFilter :: Filter,
  generalizedFilterTopper :: Topper }

noFilter :: GeneralizedFilter
noFilter = GeneralizedFilter {
  generalizedFilterFilter = NoFilter,
  generalizedFilterTopper = NoTopper }

hasNoFilter :: GeneralizedFilter -> Bool
hasNoFilter gfilter = case generalizedFilterFilter gfilter of
  NoFilter -> case generalizedFilterTopper gfilter of
               NoTopper -> True
               _ -> False
  _ -> False

applyFilter :: Filter -> TargetRecord -> Bool
applyFilter NoFilter _ = True
applyFilter (FilterByFeatures mInHeader featureSpec) tR = applyFeatureFilter mInHeader featureSpec tR

getGeneralizedFilterForScheme :: Maybe TabularHeader -> EvaluationScheme -> GeneralizedFilter
getGeneralizedFilterForScheme mTabHeader scheme = GeneralizedFilter {
  generalizedFilterFilter = getFilterForScheme mTabHeader scheme,
  generalizedFilterTopper = getTopperForScheme scheme
  }

getFilterForScheme :: Maybe TabularHeader -> EvaluationScheme -> Filter
getFilterForScheme mTabHeader (EvaluationScheme _ ops) = case findFilter ops of
  [] -> NoFilter
  fs -> FilterByFeatures mTabHeader (S.fromList $ Prelude.map (unpack . fixIndex) fs)

getTopperForScheme :: EvaluationScheme -> Topper
getTopperForScheme (EvaluationScheme _ ops) = case findTopper ops of
  [] -> NoTopper
  [topper] -> topper
  _ -> error "only one topper expected"

fixIndex = replace "[" "<" . replace "]" ">"

findFilter :: [PreprocessingOperation] -> [Text]
findFilter [] = []
findFilter ((FeatureFilter f):ops) = (f:(findFilter ops))
findFilter (_:ops) = findFilter ops

findTopper :: [PreprocessingOperation] -> [Topper]
findTopper [] = []
findTopper ((TopConfidence percentage):ops) = [Topper percentage totalRecordConfidence]
findTopper (_:ops) = findTopper ops

totalRecordConfidence :: TargetRecord -> Double
totalRecordConfidence (TargetRecord _ _ (Got out)) = case out of
  RawItemTarget t -> totalLineConfidence t
totalRecordConfidence (TargetRecord _ _ (Wrong _)) = 9999.0
totalRecordConfidence (TargetRecord _ _ Done) = -9999



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
  challengeDataSourceFilter :: GeneralizedFilter,
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
