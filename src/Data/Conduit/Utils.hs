
module Data.Conduit.Utils
  (gobbleAndDo,
   doNothing)
  where

import Data.Conduit
import Data.Conduit.Combinators


doNothing :: Monad m => ConduitT a a m ()
doNothing = Data.Conduit.Combinators.filter (const True)

gobbleAndDo :: Monad m => ([a] -> [b]) -> ConduitT a b m ()
gobbleAndDo fun = do
  l <- sinkList
  yieldMany $ fun l
