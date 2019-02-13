{-# LANGUAGE RankNTypes #-}

module Data.Conduit.SmartSource
       where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf, elemIndex, elem)
import Data.Char (ord)

import System.FilePath

import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit
import qualified Data.ByteString as S
import Data.Conduit.Binary (sourceFile, sourceHandle)
import Network.HTTP.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import qualified System.Directory as D
import System.Posix
import System.FilePath
import System.IO (stdin)
import Control.Monad ((<=<), filterM)

data SourceSpec =Stdin
                 | FilePathSpec FilePath
                 | Http String
                 | Https String
                 | GitSpec String FilePath
                 | NoSource
                deriving (Eq, Show)

recoverPath :: SourceSpec -> String
recoverPath Stdin = "-"
recoverPath (FilePathSpec filePath) = filePath
recoverPath (Http url) = url
recoverPath (Https url) = url
recoverPath (GitSpec branch filePath) = branch ++ ":" ++ filePath

data SmartSourceError = NoFile FilePath
                        | NoDirectory FilePath
                        | NoSpecGiven
                        deriving (Eq, Show)

getSmartSourceSpec :: FilePath -> FilePath -> String -> IO (Either SmartSourceError SourceSpec)
getSmartSourceSpec _ "" "" = return $ Left NoSpecGiven
getSmartSourceSpec _ _ "-" = return $ Right Stdin
getSmartSourceSpec defaultDirectory defaultFile spec
  | "http://" `isPrefixOf` spec = return $ Right $ Http spec
  | "https://" `isPrefixOf` spec = return $ Right $ Https spec
  | otherwise = do
      inDefaultDirectory <- lookForCompressedFiles (defaultDirectory </> spec)
      isInDefaultDirectory <- D.doesFileExist inDefaultDirectory
      if isInDefaultDirectory
        then
          return $ Right $ FilePathSpec inDefaultDirectory
        else
         do
          isThere <- D.doesFileExist spec
          if isThere
            then
              return $ Right $ FilePathSpec spec
            else
             do
              isDirectoryThere <- D.doesDirectoryExist defaultDirectory
              if isDirectoryThere
                then
                  return $ Left $ NoFile inDefaultDirectory
                else
                  return $ Left $ NoDirectory spec

smartSource (FilePathSpec filePath) = sourceFile filePath
smartSource Stdin = sourceHandle stdin
smartSource NoSource = error $ "should not be here"
smartSource (Http url) = httpSource url
smartSource (Https url) = httpSource url

httpSource :: MonadResource m => String -> ConduitM () S.ByteString m ()
httpSource url = do
  request <- liftIO $ parseRequest url
  manager <- liftIO $ newManager tlsManagerSettings
  response <- lift $ http request manager
  responseBody response

checkRefFormat :: String -> Bool
checkRefFormat ref =
  not ("./" `isInfixOf` ref) &&
  not (".lock" `isSuffixOf` ref) &&
  not (".lock/" `isInfixOf` ref) &&
  not (".." `isInfixOf` ref) &&
  not (any isUnwantedChar ref) &&
  not ("/" `isSuffixOf` ref) &&
  not ("//" `isInfixOf` ref) &&
  not ("@{" `isInfixOf` ref) &&
  ref /= "@"
  where isUnwantedChar ':' = True
        isUnwantedChar '?' = True
        isUnwantedChar '*' = True
        isUnwantedChar '[' = True
        isUnwantedChar '~' = True
        isUnwantedChar '^' = True
        isUnwantedChar '\\' = True
        isUnwantedChar '\177' = True
        isUnwantedChar c = ord c < 32

compressedFilesHandled = [".gz", ".xz", ".bz2"]

lookForCompressedFiles :: FilePath -> IO FilePath
lookForCompressedFiles = lookForAlternativeFiles compressedFilesHandled

lookForAlternativeFiles :: [String] -> FilePath -> IO FilePath
lookForAlternativeFiles suffixes filePath
   | takeExtension filePath `Prelude.elem` suffixes = return filePath
   | otherwise = do
       fileIsThere <- D.doesFileExist filePath
       if fileIsThere
         then
           return filePath
         else
           do
             found <- Control.Monad.filterM D.doesFileExist $ Prelude.map (filePath <.>) suffixes
             return $ case found of
                        [fp] -> fp
                        _ -> filePath
