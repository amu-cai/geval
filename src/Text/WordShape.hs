module Text.WordShape
  (WordShape(..), shapify)
  where

import Data.Text as T
import Data.Char

newtype WordShape = WordShape Text
  deriving (Eq, Ord)

instance Show WordShape where
  show (WordShape t) = unpack t

-- The idea taken from https://github.com/aleju/ner-crf/blob/master/model/features.py#L377

isBracket :: Char -> Bool
isBracket c = cat == OpenPunctuation || cat == ClosePunctuation
  where cat = generalCategory c

normalizeChar :: Char -> Char
normalizeChar c
  | isAlpha c && isUpper c = 'A'
  | isAlpha c && isLower c = 'a'
  | isDigit c              = '9'
  | isSpace c              = ' '
  | isBracket c            = '('
  | isPunctuation c        = '.'
  | otherwise              = '#'

shapify :: Text -> WordShape
shapify t = WordShape $ normalize $ T.map normalizeChar t
  where normalize t = T.reverse $ pack $ T.foldl step "" t
        step [] c = [c]
        step p '9' = '9':p
        step p@('+':h:t) c
          | h == c = p
          | otherwise = c:p
        step p@(h:t) c
          | h == c = ('+':p)
          | otherwise = c:p
