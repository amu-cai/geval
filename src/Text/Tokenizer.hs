{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Tokenizer
  where

import qualified Data.Text as T
import Data.Monoid ((<>))

import Text.Regex.PCRE.Heavy

data Tokenizer = V13a

tokenize :: Maybe Tokenizer -> T.Text -> [T.Text]
tokenize Nothing t = T.words t
tokenize (Just V13a) t = T.words tWithSpaces
  where tWithSpaces = T.strip tTokenized
        tTokenized =
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
