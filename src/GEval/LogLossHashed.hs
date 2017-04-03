{-# LANGUAGE OverloadedStrings #-}

module GEval.LogLossHashed
       (HashedDistribution, parseDistribution, calculateLogLoss)
       where

import qualified Data.Vector as V
import Control.Monad.Reader
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST

import Data.Hash.Murmur

import Data.Either

import Data.Bits
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR

import GEval.Common (textToDouble)

type HashedDistribution = V.Vector Double

murmurSeedValue :: Word32
murmurSeedValue = 0

parseDistribution :: Word32 -> Word32 -> T.Text -> Either String HashedDistribution
parseDistribution nbOfBits seed distroSpec =
  -- distribution spec is either...
  normalizeDistribution <$> if T.any (== ':') distroSpec
                            -- a space-separated list of words with their log probs
                            then parseDistributionFromWordList nbOfBits seed distroSpec
                            -- a direct list of 2^nbOfBits log probs
                            else parseDistributionFromLogProbList nbOfBits distroSpec

normalizeDistribution :: HashedDistribution -> HashedDistribution
normalizeDistribution distro =
  -- we do softmax (if needed)
  if probSum > 1.0 || probSum < (1.0 - epsilon)
  then normalized
  else distro
  where probSum = V.foldl' (\s l -> (s + exp l)) 0.0 distro
        normalized = V.map (\v -> log ((exp v) / probSum)) distro
        epsilon = 0.00000001

type DistroMonad s = ReaderT (VM.MVector s Double) (ST s)

data WordSpec = AnyWord | SpecificWord T.Text
data WordSpecWithLogProb = WordSpecWithLogProb WordSpec Double

parseDistributionFromWordList :: Word32 -> Word32 -> T.Text -> Either String HashedDistribution
parseDistributionFromWordList nbOfBits seed distroSpec = (parseDistributionFromWordList' nbOfBits seed) =<< (
  processEithers $ map getWordSpecWithLogProb $ T.splitOn " " distroSpec)

getWordSpecWithLogProb :: T.Text -> Either String WordSpecWithLogProb
getWordSpecWithLogProb t =
  if T.null wordSpecPart
  then Left "colon expected"
  else case textToDouble numberPart of
         Right lp -> Right $ WordSpecWithLogProb (getWordSpec wordSpecPart) lp
         Left m -> Left $ m
  where
    -- colon _can_ be used as a part of a word, the only character forbidden is a space
    (wordSpecPart, numberPart) = T.breakOnEnd ":" t
    -- specification like ":-1.23456" means we add probability for OOVs (or actually any word)
    getWordSpec ":" = AnyWord
    getWordSpec ws = SpecificWord $ T.dropEnd 1 ws


parseDistributionFromWordList' :: Word32 -> Word32 -> [WordSpecWithLogProb] -> Either String HashedDistribution
parseDistributionFromWordList' nbOfBits seed specs = runST $ do
  emp <- VM.replicate (hashDistributionSize nbOfBits) minusInfinity
  runReaderT (addSpecs nbOfBits seed specs) emp
  frozen <- V.freeze emp
  return $ Right frozen

addSpecs :: Word32 -> Word32 -> [WordSpecWithLogProb] -> DistroMonad s ()
addSpecs nbOfBits seed = mapM_ (updateDistro nbOfBits seed)

updateDistro :: Word32 -> Word32 -> WordSpecWithLogProb -> DistroMonad s ()
updateDistro nbOfBits seed (WordSpecWithLogProb (SpecificWord w) logProb) = do
  d <- ask
  let fp = getFingerprint nbOfBits seed w
  updateDistro' logProb fp
updateDistro nbOfBits _ (WordSpecWithLogProb AnyWord logProb) = do
  d <- ask
  -- spread probability uniformly
  mapM_ (updateDistro' (logProb - (log $ fromIntegral hSize))) [0..(hSize-1)]
  where hSize = hashDistributionSize nbOfBits

updateDistro' :: Double -> Int -> DistroMonad s ()
updateDistro' logDelta ix = do
  d <- ask
  currentLogProb <- lift $ VM.read d ix
  let newLogProb = log ((exp currentLogProb) + (exp logDelta))
  lift $ VM.write d ix newLogProb

minusInfinity :: Double
minusInfinity = log 0

parseDistributionFromLogProbList :: Word32 -> T.Text -> Either String HashedDistribution
parseDistributionFromLogProbList nbOfBits distroSpec = case parseDistributionFromLogProbList' distroSpec of
  v@(Right d) -> if length d == hashDistributionSize nbOfBits
                then v
                else Left "unexpected number of log probs"
  e@(Left _) -> e

parseDistributionFromLogProbList' :: T.Text -> Either String HashedDistribution
parseDistributionFromLogProbList' distroSpec =
  V.fromList <$> (processEithers $ map textToDouble $ T.splitOn " " distroSpec)


processEithers :: [Either a b] -> Either a [b]
processEithers es = case leftOnes of
  (firstLeftOne:_) -> Left firstLeftOne
  [] -> Right rightOnes
 where (leftOnes, rightOnes) = partitionEithers es

calculateLogLoss :: Word32 -> Word32 -> T.Text -> HashedDistribution -> Double
calculateLogLoss nbOfBits seed term distrib = distrib V.! (getFingerprint nbOfBits seed term)


hashDistributionSize :: Word32 -> Int
hashDistributionSize nbOfBits = (shift 1 (fromIntegral nbOfBits))

getFingerprint :: Word32 -> Word32 -> T.Text -> Int
getFingerprint nbOfBits seed text = fromIntegral $ h `mod` (fromIntegral $ hashDistributionSize nbOfBits)
  where h = getHash seed text

getHash :: Word32 -> T.Text -> Word32
getHash seed t = murmur3 seed bs
  where bs = TE.encodeUtf8 t
