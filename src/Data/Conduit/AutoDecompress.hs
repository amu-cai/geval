{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Conduit.AutoDecompress
       (autoDecompress)
       where

import Data.Conduit
import Data.Conduit.Combinators
import Data.ByteString
import Data.Conduit.Zlib
import Data.Word8
import Control.Monad.Trans.Resource (MonadThrow, MonadResource)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Base (MonadBase)
import qualified Data.Conduit.Lzma as XZ
import qualified Data.Conduit.BZlib as BZ

autoDecompress :: (MonadResource m, MonadBase base m, MonadThrow m, PrimMonad base) => ConduitM ByteString ByteString m ()
autoDecompress = do
  f <- await
  case f of
    Just chunk -> if Data.ByteString.length chunk > 1
                   then
                     do
                       let firstByte = Data.ByteString.head chunk
                       let secondByte = Data.ByteString.index chunk 1
                       leftover chunk
                       lookAtMagicNumbers (firstByte, secondByte)
                   else
                     do
                      leftover chunk
                      doNothing

    Nothing -> return ()


lookAtMagicNumbers :: (MonadResource m, MonadBase base m, MonadThrow m, PrimMonad base) => (Word8, Word8) -> Conduit ByteString m ByteString
lookAtMagicNumbers (31, 139) = ungzip
lookAtMagicNumbers (66, 90) = BZ.bunzip2
lookAtMagicNumbers (253, 55) = XZ.decompress Nothing
lookAtMagicNumbers _ = doNothing

doNothing :: Monad m => Conduit ByteString m ByteString
doNothing = Data.Conduit.Combinators.filter (const True)
