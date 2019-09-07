{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GEval.Annotation
       (parseAnnotations, Annotation(..),
        parseObtainedAnnotations, ObtainedAnnotation(..),
        matchScore, intSetParser)
       where

import qualified Data.IntSet as IS
import qualified Data.Text as T

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import GEval.Common (sepByWhitespaces, (/.))
import GEval.Probability
import Data.Char
import Data.Maybe (fromMaybe)

import GEval.PrecisionRecall(weightedMaxMatching)

data Annotation = Annotation T.Text IS.IntSet
                  deriving (Eq, Show)

data ObtainedAnnotation = ObtainedAnnotation Annotation Double
                          deriving (Eq, Show)

instance EntityWithProbability ObtainedAnnotation where
  type BareEntity ObtainedAnnotation = Annotation
  getBareEntity (ObtainedAnnotation annotation _) = annotation
  getProbabilityAsDouble (ObtainedAnnotation _ p) = p
  matchScore (Annotation labelA intSetA) (ObtainedAnnotation (Annotation labelB intSetB) _)
    | labelA == labelB = (intSetLength intersect) /. (intSetLength $ intSetA `IS.union` intSetB)
    | otherwise = 0.0
    where intSetLength = Prelude.length . IS.toList
          intersect = intSetA `IS.intersection` intSetB


parseObtainedAnnotations :: T.Text -> Either String [ObtainedAnnotation]
parseObtainedAnnotations t = parseOnly (obtainedAnnotationsParser <* endOfInput) t

obtainedAnnotationsParser :: Parser [ObtainedAnnotation]
obtainedAnnotationsParser = sepByWhitespaces obtainedAnnotationParser

obtainedAnnotationParser :: Parser ObtainedAnnotation
obtainedAnnotationParser = do
  annotation <- annotationParser
  mProb <- (string ":" *> double) <|> (return 1.0)
  return $ ObtainedAnnotation annotation mProb

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
