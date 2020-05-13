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

newtype Filter = Filter (Maybe (Text -> Bool))

noFilter :: Filter
noFilter = Filter Nothing

applyFilter :: Filter -> (Text, (Text, Text)) -> Bool
applyFilter (Filter Nothing) _ = True
applyFilter (Filter (Just fun)) (inp, (exp, out)) = fun inp

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
