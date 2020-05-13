module GEval.DataSource
  (ChallengeDataSource(..),
   DataSource(..),
   Filter,
   noFilter,
   applyFilter)
  where

import Data.Text

import Data.Conduit.SmartSource
import Data.Conduit.Header
import GEval.Selector

data Filter = NoFilter | InputFilter (Text -> Bool)

noFilter :: Filter
noFilter = NoFilter

applyFilter :: Filter -> (Text, (Text, Text)) -> Bool
applyFilter NoFilter _ = True
applyFilter (InputFilter fun) (inp, (exp, out)) = fun inp

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
