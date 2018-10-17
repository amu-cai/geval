{-# LANGUAGE OverloadedStrings #-}

module GEval.Annotation
       (parseAnnotations, Annotation(..), matchScore)
       where

import qualified Data.IntSet as IS
import qualified Data.Text as T

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import GEval.Common (sepByWhitespaces, (/.))
import Data.Char

data Annotation = Annotation T.Text IS.IntSet
                  deriving (Eq, Show)

parseAnnotations :: T.Text -> Either String [Annotation]
parseAnnotations t = parseOnly (annotationsParser <* endOfInput) t

annotationsParser :: Parser [Annotation]
annotationsParser = sepByWhitespaces annotationParser

annotationParser :: Parser Annotation
annotationParser = do
  label <- takeWhile1 (\c -> not (isSpace c) && c /= ':')
  string ":"
  intSet <- intSetParser
  return $ Annotation label intSet

intSetParser :: Parser IS.IntSet
intSetParser = IS.unions <$> intervalParser `sepBy` (string ",")

intervalParser :: Parser IS.IntSet
intervalParser = do
  startIx <- decimal
  endIx <- (string "-" *> decimal <|> pure startIx)
  pure $ IS.fromList [startIx..endIx]

matchScore :: Annotation -> Annotation -> Double
matchScore (Annotation labelA intSetA) (Annotation labelB intSetB)
  | labelA == labelB = (intSetLength intersect) /. (intSetLength $ intSetA `IS.union` intSetB)
  | otherwise = 0.0
  where intSetLength = Prelude.length . IS.toList
        intersect = intSetA `IS.intersection` intSetB
