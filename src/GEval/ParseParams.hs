{-# LANGUAGE OverloadedStrings #-}

module GEval.ParseParams(parseParamsFromFilePath,OutputFileParsed(..))
       where

import Data.Map.Strict as M
import Data.Text
import Data.Attoparsec.Text

import System.FilePath

data OutputFileParsed = OutputFileParsed String (M.Map Text Text)
                        deriving (Eq, Show)

parseParamsFromFilePath :: FilePath -> OutputFileParsed
parseParamsFromFilePath filePath = parseParamsFromBaseName fileBaseName
  where fileBaseName = dropExtensions $ takeBaseName filePath

parseParamsFromBaseName :: FilePath -> OutputFileParsed
parseParamsFromBaseName baseName = case parseOnly (parser <* endOfInput) (pack baseName) of
                                     (Right v) -> v
                                     (Left _) -> OutputFileParsed baseName M.empty

parser :: Parser OutputFileParsed
parser = do
  coreName <- many1 $ notChar '-'
  "-"
  paramList <- parseParamList
  return $ OutputFileParsed coreName (M.fromList paramList)


parseParamList :: Parser [(Text, Text)]
parseParamList = parseParam `sepBy` (char ',')

parseParam :: Parser (Text, Text)
parseParam = do
  param <- many1 $ satisfy (\c -> c /= '=' && c /= ',')
  "="
  val <- many1 $ notChar ','
  pure $ (strip $ pack param, strip $ pack val)
