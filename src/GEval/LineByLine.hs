{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module GEval.LineByLine
       (runLineByLine,
        runLineByLineWithWorstFeatures,
        SpanLineRecord(..),
        LineSpan(..),
        runWorstFeatures,
        runLineByLineGeneralized,
        runDiff,
        runMostWorseningFeatures,
        runDiffGeneralized,
        LineRecord(..),
        ResultOrdering(..),
        justTokenize,
        worstFeaturesPipeline,
        runOracleItemBased,
        runMultiOutputGeneralizedForEvaluationScheme
       ) where

import GEval.Core
import GEval.Common
import GEval.DataSource
import GEval.EvaluationScheme
import Text.Tokenizer

import System.IO

import Data.Conduit.Utils

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Text as CT
import Data.Text
import Data.Text.Encoding
import Data.Conduit.Rank
import Data.Maybe (fromMaybe, catMaybes)
import Data.Either (rights)

import qualified Data.Vector as V

import Data.List (sortBy, sortOn, sort, concat, maximumBy, intersperse)

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit.Lift
import Control.Monad.State.Strict

import Data.Monoid ((<>))

import GEval.FeatureExtractor
import GEval.BlackBoxDebugging
import GEval.Selector

import Data.Word

import Text.Printf

import Data.Conduit.SmartSource

import System.FilePath

import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (normalDistr)
import Data.Statistics.Kendall (kendallZ)

import Data.Conduit.Binary (sourceFile)

import Data.Conduit.Header

import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Rainbow (Chunk, magenta, cyan, fore, bold, yellow, brightYellow, red, brightRed, chunk, chunksToByteStrings, byteStringMakerFromEnvironment, byteStringMakerFromHandle)

import Data.ByteString
import Data.Function

data LineRecord = LineRecord Text Text Text Word32 MetricValue
                  deriving (Eq, Show)

readReferences :: FilePath -> IO References
readReferences referencesFilePath = do
  h <- runResourceT $ runConduit $ (sourceFile referencesFilePath
                                   .| CC.decodeUtf8Lenient
                                   .| CT.lines
                                   .| CC.map parseReferenceEntry
                                   .| CC.foldl (\h (refId, t) -> H.insert refId t h) H.empty)
  return $ References h

parseReferenceEntry :: Text -> (Integer, Text)
parseReferenceEntry line = (read $ Data.Text.unpack refId, t)
  where [refId, t] = splitOn "\t" line

runLineByLine :: ResultOrdering -> Maybe String -> GEvalSpecification -> BlackBoxDebuggingOptions -> IO ()
runLineByLine ordering featureFilter spec bbdo = do
  mInHeader <- readHeaderFileWrapper $ getInHeader spec
  runLineByLineGeneralized ordering spec (consum mInHeader)
   where consum :: Maybe TabularHeader -> Maybe References -> ConduitT LineRecord Void (ResourceT IO) ()
         consum = (\mInHeader -> \mReferences -> (runFeatureFilter featureFilter spec bbdo mReferences mInHeader .| CL.map (encodeUtf8 . formatOutput) .| CC.unlinesAscii .| CC.stdout))
         formatOutput (LineRecord inp exp out _ score) = Data.Text.intercalate "\t" [
           formatScore score,
           escapeTabs inp,
           escapeTabs exp,
           escapeTabs out]
         formatScore :: MetricValue -> Text
         formatScore = Data.Text.pack . printf "%f"

data LineSpan = UnmarkedSpan Text | MarkedSpan Double Text
  deriving (Eq, Show)

runLineByLineWithWorstFeatures :: ResultOrdering -> Maybe String -> GEvalSpecification -> BlackBoxDebuggingOptions -> IO ()
runLineByLineWithWorstFeatures ordering featureFilter spec bbdo = do
  maker <- byteStringMakerFromEnvironment
  let consum = CL.map (recordToBytes maker) .| CC.unlinesAscii .| CC.stdout
  runLineByLineWithWorstFeaturesGeneralized ordering featureFilter spec bbdo consum

recordToBytes :: (Chunk -> [ByteString] -> [ByteString]) -> SpanLineRecord -> ByteString
recordToBytes maker (SpanLineRecord inSpans expSpans outSpans score) =
  (mconcat . Data.List.intersperse "\t") [encodeUtf8 $ formatScore score,
                     lineToBytes maker inSpans,
                     lineToBytes maker expSpans,
                     lineToBytes maker outSpans]

  where formatScore :: MetricValue -> Text
        formatScore = Data.Text.pack . printf "%f"


lineToBytes :: (Chunk -> [ByteString] -> [ByteString]) -> [LineSpan] -> ByteString
lineToBytes maker spans =
  mconcat
  $ chunksToByteStrings maker
  $ Data.List.intersperse (chunk " ")
  $ Prelude.map spanToRainbowChunk $ spans

spanToRainbowChunk :: LineSpan -> Chunk
spanToRainbowChunk (UnmarkedSpan t) = chunk t
spanToRainbowChunk (MarkedSpan p t) = markedChunk p c
  where c = chunk t

markedChunk :: Double -> Chunk -> Chunk
markedChunk pValue c
  | pValue < 0.000000000000001 = bold c & fore brightRed
  | pValue < 0.000000000001 = c & fore red
  | pValue < 0.000001 = c & fore magenta
  | pValue < 0.001 = bold c
  | otherwise = c

markBadFeatures :: (M.Map PeggedFactor Double) -> (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> Text -> [LineSpan]
markBadFeatures worstFeaturesMap mTokenizer bbdo field line =
  catMaybes
  $ Prelude.map (featureToLineSpan worstFeaturesMap)
  $ extractFactors mTokenizer bbdo Nothing field line

markBadFeaturesInTabbed :: (M.Map PeggedFactor Double) -> (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> Text -> Maybe TabularHeader -> [LineSpan]
markBadFeaturesInTabbed worstFeaturesMap mTokenizer bbdo field line mInHeader =
  catMaybes
  $ Prelude.map (featureToLineSpan worstFeaturesMap)
  $ extractFactorsFromTabbed mTokenizer bbdo Nothing field line mInHeader


doMarking worstFeaturesMap mTokenizer mInHeader bbdo (LineRecord inpLine expLine outLine _ score) =
  SpanLineRecord (markBadFeaturesInTabbed worstFeaturesMap mTokenizer bbdo "in" inpLine mInHeader)
                 (markBadFeatures worstFeaturesMap mTokenizer bbdo "exp" expLine)
                 (markBadFeatures worstFeaturesMap mTokenizer bbdo "out" outLine)
                 score

featureToLineSpan :: (M.Map PeggedFactor Double) -> PeggedFactor -> Maybe LineSpan
featureToLineSpan worstFeaturesMap pf = featureToLineSpan' (M.lookup pf worstFeaturesMap) pf
  where featureToLineSpan' Nothing pf = UnmarkedSpan <$> toTextualContent pf
        featureToLineSpan' (Just pValue) pf = MarkedSpan pValue <$> toTextualContent pf

data SpanLineRecord = SpanLineRecord [LineSpan] [LineSpan] [LineSpan] MetricValue
  deriving (Eq, Show)

runLineByLineWithWorstFeaturesGeneralized :: ResultOrdering
                                            -> Maybe String
                                            -> GEvalSpecification
                                            -> BlackBoxDebuggingOptions
                                            -> ConduitT SpanLineRecord Void (ResourceT IO) r
                                            -> IO r
runLineByLineWithWorstFeaturesGeneralized ordering featureFilter spec bbdo consum = do
  System.IO.hPutStrLn stderr "Looking for worst features..."
  mInHeader <- readHeaderFileWrapper $ getInHeader spec
  worstFeatures <- runLineByLineGeneralized ordering' spec (\mReferences -> worstFeaturesPipeline False spec bbdo mReferences mInHeader (CL.take 100))
  let worstFeaturesMap = M.fromList
                         $ catMaybes
                         $ Prelude.map featureToFactor
                         $ Prelude.map (\(FeatureWithPValue feature pValue _ _) -> (feature, pValue)) worstFeatures
  mInHeader <- readHeaderFileWrapper $ getInHeader spec
  runLineByLineGeneralized ordering spec (consum' worstFeaturesMap mInHeader)
   where consum' worstFeaturesMap mInHeader = (\mReferences -> (runFeatureFilter featureFilter spec bbdo mReferences mInHeader
                                                               .| CL.map (doMarking worstFeaturesMap mTokenizer mInHeader bbdo)
                                                               .| consum))
         ordering' = forceSomeOrdering ordering
         mTokenizer = gesTokenizer spec

featureToFactor :: (Feature, Double) -> Maybe (PeggedFactor, Double)
featureToFactor ((UnaryFeature (PeggedExistentialFactor namespace (SimpleAtomicFactor factor))), p) =
  Just (PeggedFactor namespace (SimpleExistentialFactor (SimpleAtomicFactor factor)), p)
featureToFactor _ = Nothing

runFeatureFilter :: (Monad m, FeatureSource s) => Maybe String
                                                -> GEvalSpecification
                                                -> BlackBoxDebuggingOptions
                                                -> Maybe References
                                                -> Maybe TabularHeader
                                                -> ConduitT s s m ()
runFeatureFilter Nothing _ _ _ _ = doNothing
runFeatureFilter (Just feature) spec bbdo mReferences mInHeader = CC.map (\l -> (fakeRank, l))
                                                                  .| featureExtractor mTokenizer bbdo mReferences mInHeader
                                                                  .| CC.filter (checkFeature feature)
                                                                  .| CC.map fst
  where mTokenizer = gesTokenizer spec
        fakeRank = 0.0
        checkFeature feature (_, LineWithFactors _ _ fs) = feature `Prelude.elem` (Prelude.map show fs)

runWorstFeatures :: ResultOrdering -> GEvalSpecification -> BlackBoxDebuggingOptions -> IO ()
runWorstFeatures ordering spec bbdo = do
  mInHeader <- readHeaderFileWrapper $ getInHeader spec
  runLineByLineGeneralized ordering' spec (\mReferences -> worstFeaturesPipeline False spec bbdo mReferences mInHeader consumFeatures)
  where ordering' = forceSomeOrdering ordering

consumFeatures = CL.map (encodeUtf8 . formatFeatureWithPValue)
                 .| CC.unlinesAscii
                 .| CC.stdout

worstFeaturesPipeline :: Bool
                        -> GEvalSpecification
                        -> BlackBoxDebuggingOptions
                        -> Maybe References
                        -> Maybe TabularHeader
                        -> ConduitT FeatureWithPValue Void (ResourceT IO) a
                        -> ConduitT LineRecord Void (ResourceT IO) a
worstFeaturesPipeline reversed spec bbdo mReferences mInHeader consum = rank (lessByMetric reversed $ gesMainMetric spec)
                                      .| evalStateC 0 (extractFeaturesAndPValues spec bbdo mReferences mInHeader)
                                      .| CC.filter (\(FeatureWithPValue _ p _ _) -> not $ isNaN p) -- NaN values would poison sorting
                                      .| gobbleAndDo (sortBy featureOrder)
                                      .| filtreCartesian (bbdoCartesian bbdo)
                                      .| consum
  where  featureOrder (FeatureWithPValue _ p1 _ _) (FeatureWithPValue _ p2 _ _) =
           p1 `compare` p2

-- for commands like --worst-features we need some ordering (KeepTheOriginalOrder
-- does not make sense at all)
forceSomeOrdering :: ResultOrdering -> ResultOrdering
forceSomeOrdering FirstTheBest = FirstTheBest
forceSomeOrdering _ = FirstTheWorst

extractFeaturesAndPValues :: Monad m => GEvalSpecification -> BlackBoxDebuggingOptions -> Maybe References -> Maybe TabularHeader -> ConduitT (Double, LineRecord) FeatureWithPValue (StateT Integer m) ()
extractFeaturesAndPValues spec bbdo mReferences mInHeader =
  totalCounter
  .| rankedFeatureExtractor spec bbdo mReferences mInHeader
  .| uScoresCounter (bbdoMinFrequency bbdo)


data RankedFactor = RankedFactor Factor Double MetricValue
                    deriving (Show)

data FeatureWithPValue = FeatureWithPValue Feature     -- ^ feature itself
                                           Double      -- ^ p-value
                                           MetricValue -- ^ average metric value
                                           Integer     -- ^ count
                     deriving (Show)

formatFeatureWithPValue :: FeatureWithPValue -> Text
formatFeatureWithPValue (FeatureWithPValue f p avg c) =
  Data.Text.intercalate "\t" [Data.Text.pack $ show f,
                              (Data.Text.pack $ show c),
                              (Data.Text.pack $ printf "%0.8f" avg),
                              (Data.Text.pack $ printf "%0.20f" p)]

rankedFeatureExtractor :: Monad m => GEvalSpecification -> BlackBoxDebuggingOptions -> Maybe References -> Maybe TabularHeader -> ConduitT (Double, LineRecord) RankedFactor m ()
rankedFeatureExtractor spec bbdo mReferences mInHeader = featureExtractor mTokenizer bbdo mReferences mInHeader
                                                         .| CC.map snd
                                                         .| CC.map unwrapFeatures
                                                         .| CC.concat
  where mTokenizer = gesTokenizer spec
        unwrapFeatures (LineWithFactors rank score fs) = Prelude.map (\f -> RankedFactor f rank score) fs

class FeatureSource a where
  getScore :: a -> MetricValue
  mainLineRecord :: a -> LineRecord

instance FeatureSource LineRecord where
  getScore (LineRecord _ _ _ _ score) = score
  mainLineRecord l = l

instance FeatureSource (LineRecord, LineRecord) where
  getScore (LineRecord _ _ _ _ scoreA, LineRecord _ _ _ _ scoreB) = scoreB - scoreA
  mainLineRecord (_, l) = l

featureExtractor :: (Monad m, FeatureSource s) => Maybe Tokenizer -> BlackBoxDebuggingOptions -> Maybe References -> Maybe TabularHeader -> ConduitT (Double, s) (s, LineWithFactors) m ()
featureExtractor mTokenizer bbdo mReferences mInHeader = CC.map extract
                                              .| finalFeatures (bbdoCartesian bbdo) (fromMaybe (bbdoMinFrequency bbdo) (bbdoMinCartesianFrequency bbdo))
  where extract (rank, line) =
          (line, LineWithPeggedFactors rank (getScore line) $ getFeatures mTokenizer bbdo mReferences (lineToTargetRecord $ mainLineRecord line) mInHeader)

lineToTargetRecord (LineRecord inp exp out _ _) = TargetRecord (Got (RawItemTarget inp))
                                                               (Got (RawItemTarget exp))
                                                               (Got (RawItemTarget out))

finalFeatures :: Monad m => Bool -> Integer -> ConduitT (a, LineWithPeggedFactors) (a, LineWithFactors) m ()
finalFeatures False _ = CC.map (\(l, p) -> (l, peggedToUnaryLine p))
finalFeatures True minFreq = do
  ls <- CC.sinkList
  let unaryFeaturesFrequentEnough = S.fromList
                                    $ Prelude.map (\(f, c) -> f)
                                    $ Prelude.filter (\(f, c) -> c >= minFreq)
                                    $ M.toList
                                    $ M.fromListWith (+)
                                    $ Data.List.concat
                                    $ Prelude.map (\(LineWithPeggedFactors _ _ fs) -> Prelude.map (\f -> (f, 1)) $ filterExistentialFactors fs)
                                    $ Prelude.map snd ls

  (CC.yieldMany $ ls) .| CC.map (addCartesian unaryFeaturesFrequentEnough)
  where addCartesian wanted (l, LineWithPeggedFactors rank score fs) = (l, LineWithFactors rank score
                                                                           $ ((Prelude.map UnaryFactor fs) ++
                                                                              (cartesianFeatures $ Prelude.filter ((flip S.member) wanted) $ filterExistentialFactors fs)))

filtreCartesian False = CC.map id
filtreCartesian True = CC.concatMapAccum step S.empty
   where step f@(FeatureWithPValue (UnaryFeature fac) _ _ _) mp = (S.insert fac mp, [f])
         step f@(FeatureWithPValue (CartesianFeature pA pB) _ _ _) mp = (mp, if pA `S.member` mp || pB `S.member` mp
                                                                             then []
                                                                             else [f])
         step f@(FeatureWithPValue (NumericalFeature _ _ _) _ _ _) mp = (mp, [f])


peggedToUnaryLine :: LineWithPeggedFactors -> LineWithFactors
peggedToUnaryLine (LineWithPeggedFactors rank score fs) = LineWithFactors rank score (Prelude.map UnaryFactor fs)

data FeatureAggregate = ExistentialFactorAggregate Double MetricValue Integer
                        | NumericalValueAggregate [Double] [MetricValue] [Int] [MetricValue]
                        | LengthAggregate [Double] [MetricValue] [Int]

aggreggate :: FeatureAggregate -> FeatureAggregate -> FeatureAggregate
aggreggate (ExistentialFactorAggregate r1 s1 c1) (ExistentialFactorAggregate r2 s2 c2) =
  ExistentialFactorAggregate (r1 + r2) (s1 + s2) (c1 + c2)
aggreggate (NumericalValueAggregate ranks1 scores1 lengths1 values1) (NumericalValueAggregate ranks2 scores2 lengths2 values2) =
  NumericalValueAggregate (ranks1 ++ ranks2) (scores1 ++ scores2) (lengths1 ++ lengths2) (values1 ++ values2)
aggreggate (NumericalValueAggregate ranks1 scores1 lengths1 _) (LengthAggregate ranks2 scores2 lengths2) =
  LengthAggregate (ranks1 ++ ranks2) (scores1 ++ scores2) (lengths1 ++ lengths2)
aggreggate (LengthAggregate ranks1 scores1 lengths1) (NumericalValueAggregate ranks2 scores2 lengths2 _) =
  LengthAggregate (ranks1 ++ ranks2) (scores1 ++ scores2) (lengths1 ++ lengths2)
aggreggate (LengthAggregate ranks1 scores1 lengths1) (LengthAggregate ranks2 scores2 lengths2) =
  LengthAggregate (ranks1 ++ ranks2) (scores1 ++ scores2) (lengths1 ++ lengths2)
aggreggate _ _ = error "Mismatched aggregates!"

initAggregate :: RankedFactor -> (Featuroid, FeatureAggregate)
initAggregate (RankedFactor (UnaryFactor (PeggedFactor namespace (NumericalFactor Nothing l))) r s)  =
  (NumericalFeaturoid namespace, LengthAggregate [r] [s] [l])
initAggregate (RankedFactor (UnaryFactor (PeggedFactor namespace (NumericalFactor (Just v) l))) r s) =
  (NumericalFeaturoid namespace, NumericalValueAggregate [r] [s] [l] [v])
initAggregate (RankedFactor (UnaryFactor (PeggedFactor namespace (SimpleExistentialFactor f))) r s) =
  (UnaryFeaturoid (PeggedExistentialFactor namespace f), ExistentialFactorAggregate r s 1)
initAggregate (RankedFactor (CartesianFactor pA pB) r s) =
  (CartesianFeaturoid pA pB, ExistentialFactorAggregate r s 1)

filterAggregateByFreq :: Integer -> (Maybe Integer) -> FeatureAggregate -> Bool
filterAggregateByFreq minFreq Nothing (ExistentialFactorAggregate _ _ c) = c >= minFreq
filterAggregateByFreq minFreq (Just total) (ExistentialFactorAggregate _ _ c) = c >= minFreq && total - c >= minFreq
filterAggregateByFreq _ _ _ = True

uScoresCounter :: Monad m => Integer -> ConduitT RankedFactor FeatureWithPValue (StateT Integer m) ()
uScoresCounter minFreq = CC.map initAggregate
                         .| gobbleAndDo countUScores
                         .| lowerFreqFiltre
                         .| pValueCalculator minFreq
  where countUScores l =
           M.toList
           $ M.fromListWith aggreggate l
        lowerFreqFiltre = CC.filter (\(_, fAgg) -> filterAggregateByFreq minFreq Nothing fAgg)

pValueCalculator :: Monad m => Integer -> ConduitT (Featuroid, FeatureAggregate) FeatureWithPValue (StateT Integer m) ()
pValueCalculator minFreq = do
  firstVal <- await
  case firstVal of
    Just i@(_, fAgg) -> do
      total <- lift get
      if filterAggregateByFreq minFreq (Just total) fAgg
        then yield $ calculatePValue total i
        else return ()
      CC.filter (\(_, fAgg) -> filterAggregateByFreq minFreq (Just total) fAgg) .| CC.map (calculatePValue total)
    Nothing -> return ()

calculatePValue :: Integer -> (Featuroid, FeatureAggregate) -> FeatureWithPValue
calculatePValue _ (NumericalFeaturoid namespace, NumericalValueAggregate ranks scores _ values) =
                kendallPValueFeature namespace DirectValue ranks scores values
calculatePValue _ (NumericalFeaturoid namespace, LengthAggregate ranks scores lens) =
                kendallPValueFeature namespace LengthOf ranks scores lens
calculatePValue total (f, ExistentialFactorAggregate r s c) = FeatureWithPValue (featoroidToFeature f)
                                                                                (pvalue (r - minusR c) c (total - c))
                                                                                (s / (fromIntegral c))
                                                                                c
  where minusR c = (c' * (c' + 1)) / 2.0
              where c' = fromIntegral c
        -- calulating p-value from Mann–Whitney U test
        -- (normal approximation is used)
        pvalue u n1 n2 = let n1' = fromIntegral n1
                             n2' = fromIntegral n2
                             mean = n1' * n2' / 2
                             sigma = sqrt $ n1' * n2' * (n1' + n2' + 1) / 12
                             z = (u - mean) / sigma
                         in cumulative (normalDistr 0.0 1.0) z
        featoroidToFeature (UnaryFeaturoid fac) = UnaryFeature fac
        featoroidToFeature (CartesianFeaturoid facA facB) = (CartesianFeature facA facB)


kendallPValueFeature :: Ord a => FeatureNamespace -> NumericalType -> [Double] -> [MetricValue] -> [a] -> FeatureWithPValue
kendallPValueFeature namespace ntype ranks scores values = FeatureWithPValue (NumericalFeature namespace ntype ndirection)
                                                                      pv
                                                                      ((sum selectedScores) / (fromIntegral selected))
                                                                      (fromIntegral selected)
                     where z = kendallZ (V.fromList $ Prelude.zip ranks values)
                           pv = 2 * (cumulative (normalDistr 0.0 1.0) (- (abs z)))
                           ndirection = if z > 0
                                        then Small
                                        else Big
                           selected = (Prelude.length scores) `div` 4

                           selectedScores = Prelude.take selected $ Prelude.map snd $ turner $ sortOn fst $ Prelude.zip values scores
                           turner = case ndirection of
                             Small -> id
                             Big -> Prelude.reverse


totalCounter :: Monad m => ConduitT a a (StateT Integer m) ()
totalCounter = do
  m <- await
  case m of
    Just x -> do
      i <- lift get
      lift $ put $ i + 1
      yield x
      totalCounter
    Nothing -> return ()

lessByMetric :: Bool -> Metric -> (LineRecord -> LineRecord -> Bool)
lessByMetric reversed metric = lessByMetric' reversed (getMetricOrdering metric)
  where lessByMetric' False TheHigherTheBetter =
          (\(LineRecord _ _ _ _ scoreA) (LineRecord _ _ _ _ scoreB) ->
              scoreA < scoreB)
        lessByMetric' False TheLowerTheBetter =
          (\(LineRecord _ _ _ _ scoreA) (LineRecord _ _ _ _ scoreB) ->
              scoreA > scoreB)
        lessByMetric' True TheHigherTheBetter =
          (\(LineRecord _ _ _ _ scoreA) (LineRecord _ _ _ _ scoreB) ->
              scoreA > scoreB)
        lessByMetric' True TheLowerTheBetter =
          (\(LineRecord _ _ _ _ scoreA) (LineRecord _ _ _ _ scoreB) ->
              scoreA < scoreB)

runLineByLineGeneralized :: ResultOrdering
                           -> GEvalSpecification
                           -> (Maybe References -> ConduitT LineRecord Void (ResourceT IO) a)
                           -> IO a
runLineByLineGeneralized ordering spec consum = do
  mReferences <- case gesReferences spec of
    Just referencesFp -> do
      references <- readReferences referencesFp
      return $ Just references
    Nothing -> return Nothing
  dataSource' <- checkAndGetDataSource True spec
  let dataSource = addSchemeSpecifics scheme dataSource'
  gevalLineByLineCore metric dataSource (sorter ordering .| consum mReferences)
  where metric = gesMainMetric spec
        scheme = gesMainScheme spec
        sorter KeepTheOriginalOrder = doNothing
        sorter ordering = gobbleAndDo $ sortBy (sortOrder ordering (getMetricOrdering metric))
        sortOrder FirstTheWorst TheHigherTheBetter = compareScores
        sortOrder FirstTheBest TheLowerTheBetter = compareScores
        sortOrder _ _ = flip compareScores
        compareScores (LineRecord _ _ _ _ s1) (LineRecord _ _ _ _ s2) = s1 `compare` s2
        mReferences = Nothing

runDiff :: ResultOrdering -> Maybe String -> FilePath -> GEvalSpecification -> BlackBoxDebuggingOptions -> IO ()
runDiff ordering featureFilter otherOut spec bbdo = do
  mInHeader <- readHeaderFileWrapper $ getInHeader spec
  runDiffGeneralized ordering otherOut spec (consum mInHeader)
  where consum :: Maybe TabularHeader -> Maybe References -> ConduitT (LineRecord, LineRecord) Void (ResourceT IO) ()
        consum = \mInHeader -> \mReferences -> CL.filter shouldBeShown
                                             .| runFeatureFilter featureFilter spec bbdo mReferences mInHeader
                                             .| CL.map (encodeUtf8 . formatOutput)
                                             .| CC.unlinesAscii
                                             .| CC.stdout
        shouldBeShown (LineRecord _ _ outA _ scoreA, LineRecord _ _ outB _ scoreB) =
          outA /= outB && scoreA /= scoreB
        formatOutput (LineRecord inp exp outA _ scoreA, LineRecord _ _ outB _ scoreB) = Data.Text.intercalate "\t" [
          formatScoreDiff (scoreB - scoreA),
          escapeTabs inp,
          escapeTabs exp,
          escapeTabs outA,
          escapeTabs outB]
        formatScoreDiff :: Double -> Text
        formatScoreDiff = Data.Text.pack . printf "%f"

runOracleItemBased :: GEvalSpecification -> IO ()
runOracleItemBased spec = runMultiOutputGeneralized spec consum
  where consum = CL.map picker .| format
        picker = maximumBy (\(LineRecord _ _ _ _ scoreA) (LineRecord _ _ _ _ scoreB) -> metricCompare metric scoreA scoreB)
        format = CL.map (encodeUtf8 . formatOutput)
                 .| CC.unlinesAscii
                 .| CC.stdout
        formatOutput (LineRecord _ _ out _ _) = out
        metric = gesMainMetric spec

runMultiOutputGeneralized :: GEvalSpecification -> ConduitT [LineRecord] Void (ResourceT IO) () -> IO ()
runMultiOutputGeneralized spec consum = runMultiOutputGeneralizedForEvaluationScheme spec mainScheme consum
  where mainScheme = gesMainScheme spec

runMultiOutputGeneralizedForEvaluationScheme :: GEvalSpecification -> EvaluationScheme -> ConduitT [LineRecord] Void (ResourceT IO) () -> IO ()
runMultiOutputGeneralizedForEvaluationScheme spec scheme@(EvaluationScheme metric _) consum = do
  dataSource' <- checkAndGetDataSource True spec
  let dataSource = addSchemeSpecifics scheme dataSource'
  let (Just altOuts) = gesAltOutFiles spec
  altSourceSpecs' <- mapM (getSmartSourceSpec ((gesOutDirectory spec) </> (gesTestName spec)) "out.tsv") altOuts
  let altSourceSpecs = rights altSourceSpecs'
  let outSource = dataSourceOut dataSource
  let sourceSpecs = (outSource:altSourceSpecs)
  let chDataSource = dataSourceChallengeData dataSource
  let sources = Prelude.map (\s -> gevalLineByLineSource metric DataSource {
                                dataSourceChallengeData = chDataSource,
                                dataSourceOut = s}) sourceSpecs
  runResourceT $ runConduit $
    (sequenceSources sources .| consum)

runMostWorseningFeatures :: ResultOrdering -> FilePath -> GEvalSpecification -> BlackBoxDebuggingOptions -> IO ()
runMostWorseningFeatures ordering otherOut spec bbdo = do
  mInHeader <- readHeaderFileWrapper $ getInHeader spec
  runDiffGeneralized ordering' otherOut spec (consum mInHeader)
  where ordering' = forceSomeOrdering ordering
        reversed = case ordering of
          KeepTheOriginalOrder -> False
          FirstTheWorst -> False
          FirstTheBest -> True
        consum :: Maybe TabularHeader -> Maybe References -> ConduitT (LineRecord, LineRecord) Void (ResourceT IO) ()
        consum = \mInHeader -> \mReferences -> CC.map prepareFakeLineRecord
                                             .| (worstFeaturesPipeline reversed spec bbdo mReferences mInHeader consumFeatures)
        prepareFakeLineRecord :: (LineRecord, LineRecord) -> LineRecord
        prepareFakeLineRecord (LineRecord _ _ _ _ scorePrev, LineRecord inp exp out c score) =
          LineRecord inp exp out c (score - scorePrev)
        mReferences = Nothing


runDiffGeneralized :: ResultOrdering -> FilePath -> GEvalSpecification -> (Maybe References -> ConduitT (LineRecord, LineRecord) Void (ResourceT IO) a) -> IO a
runDiffGeneralized ordering otherOut spec consum = do
  dataSourceB <- checkAndGetDataSource True spec
  let dataSourceB' = addSchemeSpecifics scheme dataSourceB
  ooss <- getSmartSourceSpec ((gesOutDirectory spec) </> (gesTestName spec)) "out.tsv" otherOut
  case ooss of
    Left NoSpecGiven -> throwM $ NoOutFile otherOut
    Left (NoFile fp) -> throwM $ NoOutFile fp
    Left (NoDirectory d) -> throwM $ NoOutFile otherOut
    Right otherOutSource -> do
      let chDataSource = dataSourceChallengeData dataSourceB
      let dataSourceA = DataSource {
            dataSourceChallengeData = chDataSource,
            dataSourceOut = otherOutSource}
      let dataSourceA' = addSchemeSpecifics scheme dataSourceA
      let sourceA = gevalLineByLineSource metric dataSourceA'
      let sourceB = gevalLineByLineSource metric dataSourceB'
      runResourceT $ runConduit $
        ((getZipSource $ (,)
          <$> ZipSource sourceA
          <*> ZipSource sourceB) .| sorter ordering .| consum mReferences)
  where metric = gesMainMetric spec
        scheme = gesMainScheme spec
        preprocess = gesPreprocess spec
        mSelector = gesSelector spec
        sorter KeepTheOriginalOrder = doNothing
        sorter ordering = gobbleAndDo $ sortBy (sortOrder ordering (getMetricOrdering metric))
        sortOrder FirstTheWorst TheHigherTheBetter = compareScores
        sortOrder FirstTheBest TheLowerTheBetter = compareScores
        sortOrder _ _ = flip compareScores
        compareScores ((LineRecord _ _ _ _ o1), (LineRecord _ _ _ _ n1))
                      ((LineRecord _ _ _ _ o2), (LineRecord _ _ _ _ n2))
          = (n1 - o1) `compare` (n2 - o2)
        mReferences = Nothing


escapeTabs :: Text -> Text
escapeTabs = Data.Text.replace "\t" "<tab>"

gevalLineByLineCore :: Metric
                      -> DataSource
                      -> ConduitT LineRecord Void (ResourceT IO) a
                      -> IO a
gevalLineByLineCore metric dataSource consum =
  runResourceT $ runConduit $
     ((gevalLineByLineSource metric dataSource) .| consum)

gevalLineByLineSource :: Metric
                        -> DataSource
                        -> ConduitT () LineRecord (ResourceT IO) ()
gevalLineByLineSource metric dataSource =
  (getZipSource $ (,)
       <$> ZipSource (CL.sourceList [1..])
       <*> (ZipSource $ threeLineSource context)) .| CL.mapM (checkStepM evaluateLine) .| CL.catMaybes
  where context = fromSpecificationToWithInput lsSpec
        lsSpec = dataSourceToLineSourcesSpecification dataSource
        inputLineSource = lineSourcesInputSource lsSpec
        expectedLineSource = lineSourcesExpectedSource lsSpec
        outputLineSource = lineSourcesOutputSource lsSpec
        justLine (LineInFile _ _ l) = l

        evaluateLine (lineNo, ParsedRecordWithInput inp' exp' out') = do
          let inp = if shouldBePreprocessedForPresentation
                    then inPreprocess inp'
                    else inp'
          let exp = preprocessOut exp'
          let out = preprocessOut out'
          s <- liftIO $ gevalCoreOnSingleLines metric
                                              -- if also to be shown preprocessed, preprocessing
                                              -- will be done earlier
                                              (if shouldBePreprocessedForPresentation then id else outPreprocess)
                                              (getDataDecoder inputLineSource) (LineInFile inputSource lineNo inp)
                                              (getDataDecoder expectedLineSource) (LineInFile expectedSource lineNo exp)                                            (getDataDecoder outputLineSource) (LineInFile outSource lineNo out)
          return $ LineRecord inp exp out lineNo (extractSimpleRunValue $ getMetricValue s)
        preprocessOut = if shouldBePreprocessedForPresentation && isPreprocessable metric
                        then outPreprocess
                        else id
        challengeDataSource = dataSourceChallengeData dataSource
        mSelector = challengeDataSourceSelector challengeDataSource
        outPreprocess = challengeDataSourceOutPreprocess challengeDataSource
        inPreprocess = challengeDataSourceInPreprocess challengeDataSource
        shouldBePreprocessedForPresentation = challengeDataSourceShowPreprocessed challengeDataSource
        mInHeader = challengeDataSourceInHeader challengeDataSource
        mOutHeader = challengeDataSourceOutHeader challengeDataSource
        inputSource = challengeDataSourceInput challengeDataSource
        expectedSource = challengeDataSourceExpected challengeDataSource
        outSource = dataSourceOut dataSource


justTokenize :: Maybe Tokenizer -> IO ()
justTokenize Nothing = error "a tokenizer must be specified with --tokenizer option"
justTokenize (Just tokenizer) =
             runResourceT
             $ runConduit
             $ CC.stdin
               .| CC.decodeUtf8Lenient
               .| CT.lines
               .| CC.map (tokenizeWithSpaces (Just tokenizer))
               .| CC.unlines
               .| CC.encodeUtf8
               .| CC.stdout
