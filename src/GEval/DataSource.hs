{-# LANGUAGE OverloadedStrings #-}

module GEval.DataSource
  (ChallengeDataSource(..),
   DataSource(..),
   TargetRecord(..),
   Filter,
   noFilter,
   applyFilter)
  where

import GEval.Common (SourceItem(..))
import GEval.Selector (ItemTarget(..))

import Data.Text

import Data.Conduit.SmartSource
import Data.Conduit.Header
import GEval.Selector

data TargetRecord = TargetRecord (SourceItem ItemTarget) (SourceItem ItemTarget) (SourceItem ItemTarget)

data Filter = NoFilter | InputFilter (Text -> Bool)

noFilter :: Filter
noFilter = NoFilter

applyFilter :: Filter -> TargetRecord -> Bool
applyFilter NoFilter _ = True
applyFilter (InputFilter fun) (TargetRecord (Got (RawItemTarget t)) _ _)  = fun t
applyFilter (InputFilter fun) (TargetRecord (Got (PartiallyParsedItemTarget ts)) _ _) = fun (intercalate "\t" ts)

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
