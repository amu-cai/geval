{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Tokenizer
  where

import qualified Data.Text as T
import Data.Monoid ((<>))

import Text.Regex.PCRE.Heavy

data Tokenizer = V13a
  deriving (Eq)

instance Show Tokenizer where
  show V13a = "13a"

instance Read Tokenizer where
  readsPrec _ ('1':'3':'a':theRest) = [(V13a, theRest)]

tokenize :: Maybe Tokenizer -> T.Text -> [T.Text]
tokenize mTokenizer = T.words . (tokenizeWithSpaces mTokenizer)

tokenizeTabSeparatedWithSpaces :: Maybe Tokenizer -> T.Text -> T.Text
tokenizeTabSeparatedWithSpaces Nothing t = t -- special case for efficiency
tokenizeTabSeparatedWithSpaces tokenizer@(Just _) t =
  T.intercalate "\t"
  $ map (tokenizeWithSpaces tokenizer)
  $ T.splitOn "\t" t

tokenizeWithSpaces :: Maybe Tokenizer -> T.Text -> T.Text
tokenizeWithSpaces Nothing t = t
tokenizeWithSpaces (Just V13a) t = T.strip tTokenized
  where tTokenized =
          gsub [re|([0-9])(-)|] (\(c:p:_) -> c <> space <> p)
          $ gsub [re|([\.,])([^0-9])|] (\(c:p:_) -> c <> space <> p)
          $ gsub [re|([^0-9])([\.,])|] (\(c:p:_) -> c <> space <> p)
          $ gsub [re|[\{-\~\[-\` -\&\(-\+\:-\@\/]|] (space <>) tPadded
        tPadded = " " <> tReplaced <> " "
        tReplaced =
          T.replace "&gt;" ">"
          $ T.replace "&lt;" "<"
          $ T.replace "&amp;" "&"
          $ T.replace "&quot;" "\""
          $ T.replace "\n" " "
          $ T.replace "-\n" ""
          $ T.replace "<skipped>" "" t
        space = " " :: T.Text
