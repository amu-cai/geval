{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GEval.ParseParams(parseParamsFromFilePath,
                         parseParamsFromSourceSpec,
                         OutputFileParsed(..))
       where

import Data.Map.Strict as M
import Data.Text
import Data.Attoparsec.Text

import Text.Regex.PCRE.Heavy

import Data.Monoid ((<>))

import qualified Data.String.Utils as DSU

import System.FilePath

import Data.Conduit.SmartSource (SourceSpec(..), recoverPath)

data OutputFileParsed = OutputFileParsed String (M.Map Text Text)
                        deriving (Eq, Show)

parseParamsFromSourceSpec :: SourceSpec -> OutputFileParsed
parseParamsFromSourceSpec (FilePathSpec filePath) = parseParamsFromFilePath filePath
parseParamsFromSourceSpec spec = OutputFileParsed (recoverPath spec) M.empty

parseParamsFromFilePath :: FilePath -> OutputFileParsed
parseParamsFromFilePath filePath = parseParamsFromBaseName fileBaseName
  where fileBaseName = backDecimalSeparator $ dropExtensions $ hideDecimalSeparator $ takeBaseName filePath

dotReplacement :: Char
dotReplacement = '•'

hideDecimalSeparator :: String -> String
hideDecimalSeparator = gsub [re|([0-9])\.([0-9])|] (\(a:b:_) -> a <> [dotReplacement] <> b)

backDecimalSeparator :: String -> String
backDecimalSeparator = DSU.replace [dotReplacement] "."


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
  val <- Data.Attoparsec.Text.takeWhile (/= ',')
  pure $ (strip $ pack param, strip val)
