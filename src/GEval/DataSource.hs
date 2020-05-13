module GEval.DataSource
  (ChallengeDataSource(..),
   DataSource(..),
   Filter(..))
  where

import Data.Text

import Data.Conduit.SmartSource
import Data.Conduit.Header
import GEval.Selector

newtype Filter = Filter (Maybe (Text -> Bool))

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
