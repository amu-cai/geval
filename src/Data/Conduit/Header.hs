{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Header
       (processHeader, TabularHeader, readHeaderFile)
       where

import Data.Text
import Data.Conduit
import Data.Conduit.AutoDecompress

import qualified System.Directory as D

data TabularHeader = TabularHeader [Text]

processHeader :: Monad m => Maybe TabularHeader -> ConduitT Text Text m ()
processHeader Nothing = doNothing
processHeader (Just (TabularHeader (firstField:_))) = do
  mLine <- await
  case mLine of
    Just line -> case splitIntoFields line of
      (firstField':_) -> do
                          if firstField' == firstField
                          then return ()
                          else yield line
                          doNothing
    Nothing -> return ()

splitIntoFields :: Text -> [Text]
splitIntoFields = splitOn "\t"

readHeaderFile :: FilePath -> IO (Maybe TabularHeader)
readHeaderFile headerFilePath = do
  fileExists <- (D.doesFileExist headerFilePath)
  if fileExists
    then
     do
       content <- readFile headerFilePath
       let (firstLine:_) = Prelude.lines content
       return $ Just $ TabularHeader $ splitIntoFields $ pack firstLine
    else
       return Nothing
