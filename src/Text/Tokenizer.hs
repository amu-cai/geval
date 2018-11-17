{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Tokenizer
  where

import qualified Data.Text as T
import Data.Monoid ((<>))

import Text.Regex.PCRE.Heavy

data Tokenizer = Minimalistic | V13a | V14International
  deriving (Eq)

instance Show Tokenizer where
  show Minimalistic = "minimalistic"
  show V13a = "13a"
  show V14International = "v14"

instance Read Tokenizer where
  readsPrec _ ('m':'i':'n':'i':'m':'a':'l':'i':'s':'t':'i':'c':theRest) =
            [(Minimalistic, theRest)]
  readsPrec _ ('1':'3':'a':theRest) = [(V13a, theRest)]
  readsPrec _ ('v':'1':'4':theRest) = [(V14International, theRest)]

tokenize :: Maybe Tokenizer -> T.Text -> [T.Text]
tokenize mTokenizer = T.words . (tokenizeWithSpaces mTokenizer)

tokenizeTabSeparatedWithSpaces :: Maybe Tokenizer -> T.Text -> T.Text
tokenizeTabSeparatedWithSpaces Nothing t = t -- special case for efficiency
tokenizeTabSeparatedWithSpaces tokenizer@(Just _) t =
  T.intercalate "\t"
  $ map (tokenizeWithSpaces tokenizer)
  $ T.splitOn "\t" t

space :: T.Text
space = " "

tokenizeWithSpaces :: Maybe Tokenizer -> T.Text -> T.Text
tokenizeWithSpaces Nothing t = t
-- very simple tokenization, punctuation marks are separated
-- only at the beginning and end of a word
tokenizeWithSpaces (Just Minimalistic) t = T.strip tTokenized
  where tTokenized =
          gsub [re|\s{2,}|] ((const space) :: T.Text -> T.Text)
          $ gsub [re|[\w\d]+\S*[\w\d]+|[\w\d]|[^\w\s]+|]
                 (\tok -> space <> tok <> space)
                 t

-- tokenization following the official BLEU implementation
-- https://github.com/moses-smt/mosesdecoder/blob/master/scripts/generic/mteval-v14.pl#L954-L983
-- cf. tokenize_v14_international function in sacrebleu evaluator
tokenizeWithSpaces (Just V14International) t =
  T.strip tTokenized
  where tTokenized =
          gsub [re|\s+|] toSpace
          $ gsub [re|\p{S}|] (\s -> space <> s <> space)
          $ gsub [re|(\p{P})([^\d])|] (\(p:n:_) -> space <> p <> space <> n)
          $ gsub [re|([^\d])(\p{P})|] (\(n:p:_) -> n <> space <> p <> space) t

-- tokenization equivalent to mteval-v13a
-- cf. tokenize_13a function in sacrebleu evaluator
tokenizeWithSpaces (Just V13a) t = T.strip tTokenized
  where tTokenized =
          gsub [re|\s+|] toSpace
          $ gsub [re|([0-9])(-)|] (\(c:p:_) -> c <> space <> p <> space)
          $ gsub [re|([\.,])([^0-9])|] (\(c:p:_) -> space <> c <> space <> p)
          $ gsub [re|([^0-9])([\.,])|] (\(c:p:_) -> c <> space <> p <> space)
          $ gsub [re|[\{-\~\[-\` -\&\(-\+\:-\@\/]|] (\s -> space <> s <> space) tPadded
        tPadded = " " <> tReplaced <> " "
        tReplaced =
          T.replace "&gt;" ">"
          $ T.replace "&lt;" "<"
          $ T.replace "&amp;" "&"
          $ T.replace "&quot;" "\""
          $ T.replace "\n" " "
          $ T.replace "-\n" ""
          $ T.replace "<skipped>" "" t

toSpace :: T.Text -> T.Text
toSpace _ = space
