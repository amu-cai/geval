{-# LANGUAGE ScopedTypeVariables #-}

-- Bootstrap re-sampling

module Data.Conduit.Bootstrap
       (bootstrapC, getConfidenceBounds, defaultConfidenceLevel)
       where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import Data.Vector
import qualified Data.Vector.Generic as VG
import Data.List (sort)

import System.Random.TF (mkTFGen)
import System.Random (randomRs)

bootstrapC :: Monad m => Int -> ConduitT c Void (ResourceT m) f -> ConduitT c Void (ResourceT m) [f]
bootstrapC numberOfSamples final = do
  l <- CC.sinkList
  let v = fromList l
  results <- Prelude.mapM (\i -> (CC.yieldMany (resampleVector (mkTFGen i) v) .| final)) [1..numberOfSamples]

  return results

resampleVector gen v = Prelude.map (\ix -> v VG.! ix) $ Prelude.take n $ randomRs (0, n-1) gen
  where n = VG.length v

defaultConfidenceLevel = 0.95

getConfidenceBounds :: Ord a => Double -> [a] -> (a, a)
getConfidenceBounds confidenceLevel samples = ((samplesSorted !! toBeCut), (samplesSorted !! (n - 1 - toBeCut)))
  where n = Prelude.length samples
        toBeCut' = floor (((1 - confidenceLevel + epsilon) * (fromIntegral n)) / 2)
        toBeCut = if 2 * toBeCut' >= n
                  then toBeCut' - 1
                  else toBeCut'
        samplesSorted = sort samples
        epsilon = 0.0001
