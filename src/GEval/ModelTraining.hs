{-# LANGUAGE ScopedTypeVariables #-}

module GEval.ModelTraining(
  trainModel,
  infer)
  where

import GEval.Core
import GEval.Model
import GEval.Common

import System.FilePath
import Data.Either
import Data.Maybe

import Data.List (maximumBy, transpose)

import Data.Conduit.Header (processHeader)
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light.Base (Regex(..))
import Data.Conduit
import Data.Conduit.AutoDecompress (autoDecompress)
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import qualified Data.Yaml as Y

import Data.Conduit.SmartSource
import GEval.EvaluationScheme

infer :: FilePath -> GEvalSpecification -> IO ()
infer modelPath spec = do
  model :: SimpleMixEnsembleModel <- Y.decodeFileThrow modelPath
  outs <- fromJust <$> checkMultipleOuts spec
  outSpecs' <- mapM (getSmartSourceSpec ((gesOutDirectory spec) </> (gesTestName spec)) "out.tsv") outs
  let outSpecs = rights outSpecs'
  mHeader <- readHeaderFileWrapper headerSpec
  let bareFilesMap = M.fromList $ zip (map (\(FilePathSpec fp) -> takeFileName fp) outSpecs) [0..]

  let sources = map (createSource mHeader) outSpecs
  runResourceT $ runConduit $ (sequenceSources sources .| (CC.map (runModel model bareFilesMap)) .| CC.unlines .| CC.encodeUtf8 .| CC.stdout)
  where createSource mHeader spec = (smartSource spec) .| autoDecompress .| CT.decodeUtf8Lenient .| CT.lines .| processHeader mHeader
        headerSpec = gesOutHeader spec

runModel :: SimpleMixEnsembleModel -> M.Map FilePath Int -> [T.Text] -> T.Text
runModel (SimpleMixEnsembleModel rules) bareFilesMap ts = T.intercalate (T.pack " ") $ map applyRule rules
  where applyRule rule = applyRegex (simpleMixEnsembleModelRuleRegex rule) (ts !! (bareFilesMap M.! (simpleMixEnsembleModelRuleOut rule)))
        applyRegex regex = T.concat . (map fst) . (scan regex)


trainModel :: ModelType -> GEvalSpecification -> IO ()
trainModel SimpleMixEnsemble spec = do
  vals' <- geval spec
  let vals = map (\(s, val) -> (s, map getMetricValue val)) vals'

  let byMetric = filter (\(s, _) -> isEligibleForSimpleMix s) $ zip (gesMetrics spec) $ transpose $ map (\(_, scores) -> scores) vals

  let sources = map (\(FilePathSpec fp, _) -> takeFileName fp) vals

  let bestOnes = map ((\(scheme, ix) -> SimpleMixEnsembleModelRule {
                                           simpleMixEnsembleModelRuleRegex = fromJust $ getRegexpMatch scheme,
                                           simpleMixEnsembleModelRuleOut = sources !! ix}) . selectBest) byMetric

  let model = SimpleMixEnsembleModel bestOnes
  let modelContent = Y.encode model
  BS.putStr modelContent
  where selectBest (scheme, scores) = (scheme, bestIx)
          where bestIx = fst $ maximumBy (\(_, SimpleRun s) (_, SimpleRun s') -> s `compare` s') $ zip [0..] scores

isEligibleForSimpleMix :: EvaluationScheme -> Bool
isEligibleForSimpleMix scheme =
  evaluationSchemePriority scheme == 2 && isJust (getRegexpMatch scheme)
