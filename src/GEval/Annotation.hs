{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GEval.Annotation
       (parseAnnotations, Annotation(..),
        parseObtainedAnnotations, ObtainedAnnotation(..),
        matchScore, intSetParser, segmentAccuracy, parseSegmentAnnotations)
       where

import qualified Data.IntSet as IS
import qualified Data.Text as T
import Data.Set (intersection, fromList)

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import GEval.Common (sepByWhitespaces, (/.))
import GEval.Probability
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)

import GEval.PrecisionRecall(weightedMaxMatching)

data Annotation = Annotation T.Text IS.IntSet
                  deriving (Eq, Show, Ord)

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

parseSegmentAnnotations :: T.Text -> Either String [Annotation]
parseSegmentAnnotations t = case parseAnnotationsWithColons t of
  Left m -> Left m
  Right annotations -> if areSegmentsDisjoint annotations
                      then (Right annotations)
                      else (Left "Overlapping segments")

areSegmentsDisjoint :: [Annotation] -> Bool
areSegmentsDisjoint = areIntSetsDisjoint . map (\(Annotation _ s) -> s)

areIntSetsDisjoint :: [IS.IntSet] -> Bool
areIntSetsDisjoint ss = snd $ foldr step (IS.empty, True) ss
  where step _ w@(_, False) = w
        step s (u, True) = (s `IS.union` u, s `IS.disjoint` u)

-- unfortunately, attoparsec does not seem to back-track properly
-- so we need a special function if labels can contain colons
parseAnnotationsWithColons :: T.Text -> Either String [Annotation]
parseAnnotationsWithColons t = case partitionEithers (map parseAnnotationWithColons $ T.words t) of
  ([], annotations) -> Right annotations
  ((firstProblem:_), _) -> Left firstProblem

parseAnnotationWithColons :: T.Text -> Either String Annotation
parseAnnotationWithColons t = if T.null label
                              then Left "Colon expected"
                              else case parseOnly (intSetParser <* endOfInput) position of
                                     Left m -> Left m
                                     Right s -> Right (Annotation (T.init label) s)
  where (label, position) = T.breakOnEnd ":" t

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

segmentAccuracy :: [Annotation] -> [Annotation] -> Double
segmentAccuracy expected output = (fromIntegral $ length matched) / (fromIntegral $ length expected)
  where matched = (fromList expected) `intersection` (fromList output)
