{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module GEval.LineByLine
       (runLineByLine,
        runWorstFeatures,
        runLineByLineGeneralized,
        runDiff,
        runMostWorseningFeatures,
        runDiffGeneralized,
        LineRecord(..),
        ResultOrdering(..),
        justTokenize
       ) where

import GEval.Core
import GEval.Common
import Text.Tokenizer

import Data.Conduit.AutoDecompress (doNothing)

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Text as CT
import Data.Text
import Data.Text.Encoding
import Data.Conduit.Rank
import Data.Maybe (fromMaybe)

import Data.List (sortBy, sort, concat)

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit.Lift
import Control.Monad.State.Strict

import Data.Monoid ((<>))

import GEval.FeatureExtractor
import GEval.BlackBoxDebugging

import Data.Word

import Text.Printf

import Data.Conduit.SmartSource

import System.FilePath

import Statistics.Distribution (cumulative)
import Statistics.Distribution.Normal (normalDistr)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data LineRecord = LineRecord Text Text Text Word32 MetricValue
                  deriving (Eq, Show)

runLineByLine :: ResultOrdering -> Maybe String -> GEvalSpecification -> BlackBoxDebuggingOptions -> IO ()
runLineByLine ordering featureFilter spec bbdo = runLineByLineGeneralized ordering spec consum
   where consum :: ConduitT LineRecord Void (ResourceT IO) ()
         consum = (runFeatureFilter featureFilter spec bbdo .| CL.map (encodeUtf8 . formatOutput) .| CC.unlinesAscii .| CC.stdout)
         formatOutput (LineRecord inp exp out _ score) = Data.Text.intercalate "\t" [
           formatScore score,
           escapeTabs inp,
           escapeTabs exp,
           escapeTabs out]
         formatScore :: MetricValue -> Text
         formatScore = Data.Text.pack . printf "%f"

runFeatureFilter :: (Monad m, FeatureSource s) => Maybe String -> GEvalSpecification -> BlackBoxDebuggingOptions -> ConduitT s s m ()
runFeatureFilter Nothing _ _ = doNothing
runFeatureFilter (Just feature) spec bbdo = CC.map (\l -> (fakeRank, l))
                                            .| featureExtractor mTokenizer bbdo
                                            .| CC.filter (checkFeature feature)
                                            .| CC.map fst
  where mTokenizer = gesTokenizer spec
        fakeRank = 0.0
        checkFeature feature (_, LineWithFeatures _ _ fs) = feature `elem` (Prelude.map show fs)

runWorstFeatures :: ResultOrdering -> GEvalSpecification -> BlackBoxDebuggingOptions -> IO ()
runWorstFeatures ordering spec bbdo = runLineByLineGeneralized ordering' spec (worstFeaturesPipeline False spec bbdo)
  where ordering' = forceSomeOrdering ordering



worstFeaturesPipeline :: Bool -> GEvalSpecification -> BlackBoxDebuggingOptions -> ConduitT LineRecord Void (ResourceT IO) ()
worstFeaturesPipeline reversed spec bbdo = rank (lessByMetric reversed $ gesMainMetric spec)
                                      .| evalStateC 0 (extractFeaturesAndPValues spec bbdo)
                                      .| gobbleAndDo (sortBy featureOrder)
                                      .| filtreCartesian (bbdoCartesian bbdo)
                                      .| CL.map (encodeUtf8 . formatFeatureWithPValue)
                                      .| CC.unlinesAscii
                                      .| CC.stdout
  where  formatOutput (LineRecord inp exp out _ score) = Data.Text.intercalate "\t" [
           formatScore score,
           escapeTabs inp,
           escapeTabs exp,
           escapeTabs out]
         formatScore :: MetricValue -> Text
         formatScore = Data.Text.pack . printf "%f"
         featureOrder (FeatureWithPValue _ p1 _ _) (FeatureWithPValue _ p2 _ _) =
           p1 `compare` p2

-- for commands like --worst-features we need some ordering (KeepTheOriginalOrder
-- does not make sense at all)
forceSomeOrdering :: ResultOrdering -> ResultOrdering
forceSomeOrdering FirstTheBest = FirstTheBest
forceSomeOrdering _ = FirstTheWorst

extractFeaturesAndPValues :: Monad m => GEvalSpecification -> BlackBoxDebuggingOptions -> ConduitT (Double, LineRecord) FeatureWithPValue (StateT Integer m) ()
extractFeaturesAndPValues spec bbdo =
  totalCounter
  .| rankedFeatureExtractor spec bbdo
  .| uScoresCounter (bbdoMinFrequency bbdo)


data RankedFeature = RankedFeature Feature Double MetricValue
                     deriving (Show)

data FeatureWithPValue = FeatureWithPValue Feature     -- ^ feature itself
                                           Double      -- ^ p-value
                                           MetricValue -- ^ average metric value
                                           Integer     -- ^ count
                     deriving (Show)

formatFeatureWithPValue :: FeatureWithPValue -> Text
formatFeatureWithPValue (FeatureWithPValue f p avg c) =
  Data.Text.intercalate "\t" [pack $ show f,
                              (pack $ show c),
                              (pack $ printf "%0.8f" avg),
                              (pack $ printf "%0.20f" p)]

rankedFeatureExtractor :: Monad m => GEvalSpecification -> BlackBoxDebuggingOptions -> ConduitT (Double, LineRecord) RankedFeature m ()
rankedFeatureExtractor spec bbdo = featureExtractor mTokenizer bbdo
                                   .| CC.map snd
                                   .| CC.map unwrapFeatures
                                   .| CC.concat
  where mTokenizer = gesTokenizer spec
        unwrapFeatures (LineWithFeatures rank score fs) = Prelude.map (\f -> RankedFeature f rank score) fs

class FeatureSource a where
  getScore :: a -> MetricValue
  mainLineRecord :: a -> LineRecord

instance FeatureSource LineRecord where
  getScore (LineRecord _ _ _ _ score) = score
  mainLineRecord l = l

instance FeatureSource (LineRecord, LineRecord) where
  getScore (LineRecord _ _ _ _ scoreA, LineRecord _ _ _ _ scoreB) = scoreB - scoreA
  mainLineRecord (_, l) = l

featureExtractor :: (Monad m, FeatureSource s) => Maybe Tokenizer -> BlackBoxDebuggingOptions -> ConduitT (Double, s) (s, LineWithFeatures) m ()
featureExtractor mTokenizer bbdo = CC.map extract
                                   .| finalFeatures (bbdoCartesian bbdo) (fromMaybe (bbdoMinFrequency bbdo) (bbdoMinCartesianFrequency bbdo))
  where extract (rank, line) =
          (line, LineWithPeggedFactors rank (getScore line) $ getFeatures mTokenizer bbdo (mainLineRecord line))

finalFeatures :: Monad m => Bool -> Integer -> ConduitT (a, LineWithPeggedFactors) (a, LineWithFeatures) m ()
finalFeatures False _ = CC.map (\(l, p) -> (l, peggedToUnaryLine p))
finalFeatures True minFreq = do
  ls <- CC.sinkList
  let unaryFeaturesFrequentEnough = S.fromList
                                    $ Prelude.map (\(f, c) -> f)
                                    $ Prelude.filter (\(f, c) -> c >= minFreq)
                                    $ M.toList
                                    $ M.fromListWith (+)
                                    $ Data.List.concat
                                    $ Prelude.map (\(LineWithPeggedFactors _ _ fs) -> Prelude.map (\f -> (f, 1)) fs)
                                    $ Prelude.map snd ls

  (CC.yieldMany $ ls) .| CC.map (addCartesian unaryFeaturesFrequentEnough)
  where addCartesian wanted (l, LineWithPeggedFactors rank score fs) = (l, LineWithFeatures rank score
                                                                           $ ((Prelude.map UnaryFeature fs) ++
                                                                              (cartesianFeatures $ Prelude.filter ((flip S.member) wanted) fs)))

filtreCartesian False = CC.map id
filtreCartesian True = CC.concatMapAccum step S.empty
   where step f@(FeatureWithPValue (UnaryFeature p) _ _ _) mp = (S.insert p mp, [f])
         step f@(FeatureWithPValue (CartesianFeature pA pB) _ _ _) mp = (mp, if pA `S.member` mp || pB `S.member` mp
                                                      then []
                                                      else [f])

peggedToUnaryLine :: LineWithPeggedFactors -> LineWithFeatures
peggedToUnaryLine (LineWithPeggedFactors rank score fs) = LineWithFeatures rank score (Prelude.map UnaryFeature fs)

getFeatures :: Maybe Tokenizer -> BlackBoxDebuggingOptions -> LineRecord -> [PeggedFactor]
getFeatures mTokenizer bbdo (LineRecord inLine expLine outLine _ _) =
  Data.List.concat [
     extractFactors mTokenizer bbdo "exp" expLine,
     extractFactorsFromTabbed mTokenizer bbdo "in" inLine,
     extractFactors mTokenizer bbdo "out" outLine]

uScoresCounter :: Monad m => Integer -> ConduitT RankedFeature FeatureWithPValue (StateT Integer m) ()
uScoresCounter minFreq = CC.map (\(RankedFeature feature r score) -> (feature, (r, score, 1)))
                         .| gobbleAndDo countUScores
                         .| lowerFreqFiltre
                         .| pValueCalculator minFreq
  where countUScores l =
           M.toList
           $ M.fromListWith (\(r1, s1, c1) (r2, s2, c2) -> ((r1 + r2), (s1 + s2), (c1 + c2))) l
        lowerFreqFiltre = CC.filter (\(_, (_, _, c)) -> c >= minFreq)

pValueCalculator :: Monad m => Integer -> ConduitT (Feature, (Double, MetricValue, Integer)) FeatureWithPValue (StateT Integer m) ()
pValueCalculator minFreq = do
  firstVal <- await
  case firstVal of
    Just i@(_, (_, _, c)) -> do
      total <- lift get
      if total - c >= minFreq
        then yield $ calculatePValue total i
        else return ()
      CC.filter (\(_, (_, _, c)) -> total - c >= minFreq) .| CC.map (calculatePValue total)
    Nothing -> return ()

calculatePValue :: Integer -> (Feature, (Double, MetricValue, Integer)) -> FeatureWithPValue
calculatePValue total (f, (r, s, c)) = FeatureWithPValue f
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

runLineByLineGeneralized :: ResultOrdering -> GEvalSpecification -> ConduitT LineRecord Void (ResourceT IO) a -> IO a
runLineByLineGeneralized ordering spec consum = do
  (inputFilePath, expectedFilePath, outFilePath) <- checkAndGetFilesSingleOut True spec
  gevalLineByLineCore metric preprocess inputFilePath expectedFilePath outFilePath (sorter ordering .| consum)
  where metric = gesMainMetric spec
        preprocess = gesPreprocess spec
        sorter KeepTheOriginalOrder = doNothing
        sorter ordering = gobbleAndDo $ sortBy (sortOrder ordering (getMetricOrdering metric))
        sortOrder FirstTheWorst TheHigherTheBetter = compareScores
        sortOrder FirstTheBest TheLowerTheBetter = compareScores
        sortOrder _ _ = flip compareScores
        compareScores (LineRecord _ _ _ _ s1) (LineRecord _ _ _ _ s2) = s1 `compare` s2

gobbleAndDo :: Monad m => ([a] -> [b]) -> ConduitT a b m ()
gobbleAndDo fun = do
  l <- CC.sinkList
  CC.yieldMany $ fun l

runDiff :: ResultOrdering -> Maybe String -> FilePath -> GEvalSpecification -> BlackBoxDebuggingOptions -> IO ()
runDiff ordering featureFilter otherOut spec bbdo = runDiffGeneralized ordering otherOut spec consum
  where consum :: ConduitT (LineRecord, LineRecord) Void (ResourceT IO) ()
        consum = CL.filter shouldBeShown
                 .| runFeatureFilter featureFilter spec bbdo
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

runMostWorseningFeatures :: ResultOrdering -> FilePath -> GEvalSpecification -> BlackBoxDebuggingOptions -> IO ()
runMostWorseningFeatures ordering otherOut spec bbdo = runDiffGeneralized ordering' otherOut spec consum
  where ordering' = forceSomeOrdering ordering
        reversed = case ordering of
          KeepTheOriginalOrder -> False
          FirstTheWorst -> False
          FirstTheBest -> True
        consum :: ConduitT (LineRecord, LineRecord) Void (ResourceT IO) ()
        consum = CC.map prepareFakeLineRecord
                 .| (worstFeaturesPipeline reversed spec bbdo)
        prepareFakeLineRecord :: (LineRecord, LineRecord) -> LineRecord
        prepareFakeLineRecord (LineRecord _ _ _ _ scorePrev, LineRecord inp exp out c score) =
          LineRecord inp exp out c (score - scorePrev)


runDiffGeneralized :: ResultOrdering -> FilePath -> GEvalSpecification -> ConduitT (LineRecord, LineRecord) Void (ResourceT IO) a -> IO a
runDiffGeneralized ordering otherOut spec consum = do
  (inputSource, expectedSource, outSource) <- checkAndGetFilesSingleOut True spec
  ooss <- getSmartSourceSpec ((gesOutDirectory spec) </> (gesTestName spec)) "out.tsv" otherOut
  case ooss of
    Left NoSpecGiven -> throwM $ NoOutFile otherOut
    Left (NoFile fp) -> throwM $ NoOutFile fp
    Left (NoDirectory d) -> throwM $ NoOutFile otherOut
    Right otherOutSource -> do
      let sourceA = gevalLineByLineSource metric preprocess inputSource expectedSource otherOutSource
      let sourceB = gevalLineByLineSource metric preprocess inputSource expectedSource outSource
      runResourceT $ runConduit $
        ((getZipSource $ (,)
          <$> ZipSource sourceA
          <*> ZipSource sourceB) .| sorter ordering .| consum)
  where metric = gesMainMetric spec
        preprocess = gesPreprocess spec
        sorter KeepTheOriginalOrder = doNothing
        sorter ordering = gobbleAndDo $ sortBy (sortOrder ordering (getMetricOrdering metric))
        sortOrder FirstTheWorst TheHigherTheBetter = compareScores
        sortOrder FirstTheBest TheLowerTheBetter = compareScores
        sortOrder _ _ = flip compareScores
        compareScores ((LineRecord _ _ _ _ o1), (LineRecord _ _ _ _ n1))
                      ((LineRecord _ _ _ _ o2), (LineRecord _ _ _ _ n2))
          = (n1 - o1) `compare` (n2 - o2)


escapeTabs :: Text -> Text
escapeTabs = Data.Text.replace "\t" "<tab>"

gevalLineByLineCore :: Metric -> (Text -> Text) -> SourceSpec -> SourceSpec -> SourceSpec -> ConduitT LineRecord Void (ResourceT IO) a -> IO a
gevalLineByLineCore metric preprocess inputSource expectedSource outSource consum =
  runResourceT $ runConduit $
     ((gevalLineByLineSource metric preprocess inputSource expectedSource outSource) .| consum)

gevalLineByLineSource :: Metric -> (Text -> Text) -> SourceSpec -> SourceSpec -> SourceSpec -> ConduitT () LineRecord (ResourceT IO) ()
gevalLineByLineSource metric preprocess inputSource expectedSource outSource =
  (getZipSource $ (,)
       <$> ZipSource (CL.sourceList [1..])
       <*> (ZipSource $ recordSource context parserSpec)) .| CL.mapM (checkStepM evaluateLine) .| CL.catMaybes
  where parserSpec = (ParserSpecWithInput (Right . id) (Right . id) (Right . id))
        context = (WithInput inputLineSource expectedLineSource outputLineSource)
        inputLineSource = fileAsLineSource inputSource id
        expectedLineSource = fileAsLineSource expectedSource id
        outputLineSource = fileAsLineSource outSource id
        justLine (LineInFile _ _ l) = l
        evaluateLine (lineNo, ParsedRecordWithInput inp exp out) = do
          s <- liftIO $ gevalCoreOnSingleLines metric preprocess (LineInFile inputSource lineNo inp)
                                                                (LineInFile expectedSource lineNo exp)
                                                                (LineInFile outSource lineNo out)
          return $ LineRecord inp exp out lineNo s

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
