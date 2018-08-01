{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Conduit.Rank
       (rank)
       where

import Data.Conduit

data PreviousStuff a = None | Cached [a]

rank :: Monad m => (a -> a -> Bool) -> ConduitT a (Double, a) m ()
rank less = rank' less 1.0 None

rank' :: Monad m => (a -> a -> Bool) -> Double -> PreviousStuff a -> ConduitT a (Double, a) m ()
rank' less r ps =  do
    mx <- await
    case mx of
      Just x ->
        case ps of
          None -> do
            rank' less r $ Cached [x]
          Cached s@(h:_) -> do
            if h `less` x
              then
               do
                yieldBatch r s
                rank' less (r + (fromIntegral $ length s)) $ Cached [x]
              else
                rank' less r $ Cached (x:s)
      Nothing ->
        case ps of
          None -> return ()
          Cached s -> yieldBatch r s

yieldBatch :: Monad m => Double -> [a] -> ConduitT a (Double, a) m ()
yieldBatch r s = mapM_ (\x -> yield (r', x)) $ reverse s
  where r' = (r + (r + (fromIntegral $ (length s - 1)))) / 2.0
