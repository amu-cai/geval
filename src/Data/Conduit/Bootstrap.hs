{-# LANGUAGE ScopedTypeVariables #-}

-- Bootstrap re-sampling

module Data.Conduit.Bootstrap
       (bootstrapC)
       where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import Data.Vector.Unboxed
import qualified Data.Vector.Generic as VG

import Debug.Trace

import System.Random (mkStdGen, randomRs)

bootstrapC :: (Show c, Show f, Unbox c, Monad m) => Int -> ConduitT c Void (ResourceT m) f -> ConduitT c Void (ResourceT m) [f]
bootstrapC numberOfSamples final = do
  l <- CC.sinkList
  let v = fromList l
  results <- Prelude.mapM (\i -> (CC.yieldMany (resampleVector (mkStdGen i) v) .| final)) [1..numberOfSamples]
  return results

resampleVector gen v = Prelude.map (\ix -> v VG.! ix) $ Prelude.take n $ randomRs (0, n-1) gen
  where n = VG.length v
