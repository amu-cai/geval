{-# LANGUAGE RankNTypes #-}

module Data.Conduit.SmartSource
       where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf, elemIndex, elem)
import Data.Char (ord)

import System.FilePath

import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit
import qualified Data.ByteString as S
import Data.Conduit.Binary (sourceFile)
import Network.HTTP.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)

data SmartSpec = NoSpec
                 | Stdin
                 | FileNameSpec FilePath
                 | FilePathSpec FilePath
                 | Http String
                 | Https String
                 | GitSpec String (Maybe FilePath)
                 | PossiblyGitSpec String
                 deriving (Eq, Show)

--smartSource :: (MonadIO m, MonadResource m) => [FilePath] -> Maybe FilePath -> SmartSpec -> Producer m S.ByteString
smartSource defaultDirs defaultFile spec = pureSmartSource defaultDirs spec

--pureSmartSource :: (MonadIO m, MonadResource m) => [FilePath] -> SmartSpec -> Producer m S.ByteString
pureSmartSource _ NoSpec = error "No source specification given"
pureSmartSource _ (FileNameSpec fileName) = sourceFile fileName
pureSmartSource _ (FilePathSpec fileName) = sourceFile fileName
pureSmartSource [] (PossiblyGitSpec spec) = sourceFile spec
pureSmartSource (firstDir:_) (PossiblyGitSpec spec) = sourceFile (firstDir </> spec)
pureSmartSource _ (Https url) = httpSource url
pureSmartSource _ (Http url) = httpSource url

httpSource :: MonadResource m => String -> ConduitM () S.ByteString m ()
httpSource url = do
  request <- liftIO $ parseRequest url
  manager <- liftIO $ newManager tlsManagerSettings
  response <- lift $ http request manager
  (httpsource, finalizer) <- lift $ unwrapResumable (responseBody response)
  httpsource
  lift finalizer

parseSmartSpec :: FilePath -> SmartSpec
parseSmartSpec "" = NoSpec
parseSmartSpec "-" = Stdin
parseSmartSpec spec
  | "http://" `isPrefixOf` spec = Http spec
  | "https://" `isPrefixOf` spec = Https spec
  | otherwise = case elemIndex ':' spec of
      Just ix -> let ref = take ix spec in
                if checkRefFormat ref
                     then
                       GitSpec ref (if ix == length spec - 1
                                      then
                                         Nothing
                                      else
                                         Just $ drop (ix+1) spec)
                     else
                       fileSpec
      Nothing -> if checkRefFormat spec && not ('/' `elem` spec) && not ('.' `elem` spec)
                then
                  PossiblyGitSpec spec
                else
                  fileSpec
  where fileSpec = (if '/' `elem` spec then FilePathSpec else FileNameSpec) spec

parseSmartSpecInContext :: [FilePath] -> Maybe FilePath -> String -> Maybe SmartSpec
parseSmartSpecInContext defaultDirs defaultFile spec = parseSmartSpecInContext' defaultDirs defaultFile $ parseSmartSpec spec
  where  parseSmartSpecInContext' _ Nothing NoSpec = Nothing
         parseSmartSpecInContext' [] (Just defaultFile) NoSpec = Just $ FileNameSpec defaultFile
         parseSmartSpecInContext' (firstDir:_) (Just defaultFile) NoSpec = Just $ FilePathSpec (firstDir </> defaultFile)

         parseSmartSpecInContext' (firstDir:_) _ (FileNameSpec fileName) = Just $ FilePathSpec (firstDir </> fileName)

         parseSmartSpecInContext' _ Nothing (GitSpec branch Nothing) = Nothing
         parseSmartSpecInContext' [] (Just defaultFile) (GitSpec branch Nothing) = Just $ GitSpec branch $ Just defaultFile
         parseSmartSpecInContext' (firstDir:_) (Just defaultFile) (GitSpec branch Nothing)
           = Just $ GitSpec branch $ Just (firstDir </> defaultFile)

         parseSmartSpecInContext' _ _ parsedSpec = Just parsedSpec

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
