{-# LANGUAGE OverloadedStrings #-}

module GEval.Clippings
       where

import Data.Attoparsec.Text
import Data.Text
import Control.Applicative
import Control.Exception
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)

import Debug.Trace

import GEval.Common

newtype PageNumber = PageNumber Int
        deriving (Eq, Show)

data Point = Point Int Int
             deriving (Show, Eq)

data Rectangle = Rectangle Point Point
                 deriving (Show, Eq)

data Clipping = Clipping PageNumber Rectangle
                deriving (Show, Eq)

data ClippingSpec = ClippingSpec PageNumber Rectangle Rectangle
                    deriving (Show, Eq)

data LabeledClipping = LabeledClipping (Maybe Text) Clipping
                       deriving (Show, Eq)

matchClippingToSpec :: ClippingSpec -> Clipping -> Bool
matchClippingToSpec (ClippingSpec pageNo (Rectangle (Point x0' y0') (Point x1' y1'))
                                         (Rectangle (Point x0'' y0'') (Point x1'' y1'')))
                    (Clipping pageNo' (Rectangle (Point x0 y0) (Point x1 y1))) =
  pageNo == pageNo' &&
    isInside x0 x0' x0'' && isInside y0 y0' y0'' && isInside x1 x1' x1'' && isInside y1 y1' y1''
  where isInside c c' c'' = c >= c' && c <= c'' || c <= c' && c >= c''


lineClippingsParser :: Parser [Clipping]
lineClippingsParser = sepByWhitespaces clippingParser

clippingParser :: Parser Clipping
clippingParser = do
  pageNo <- PageNumber <$> decimal
  char '/'
  rectangle <- rectangleParser
  return $ Clipping pageNo rectangle

lineClippingSpecsParser :: Parser [ClippingSpec]
lineClippingSpecsParser = sepByWhitespaces clippingSpecParser

clippingSpecParser :: Parser ClippingSpec
clippingSpecParser = do
  pageNo <- PageNumber <$> decimal
  char '/'
  rectangle <- rectangleParser
  char '/'
  margin <- decimal
  return $ ClippingSpec pageNo (smallerRectangle margin rectangle) (extendedRectangle margin rectangle)

labeledClippingParser :: Parser LabeledClipping
labeledClippingParser =
  choice [clippingWithLabelParser,
          (LabeledClipping Nothing <$> clippingParser)]

clippingWithLabelParser :: Parser LabeledClipping
clippingWithLabelParser = do
  label <- takeWhile1 (\c -> not (isSpace c) && c /= ':')
  string ":"
  clipping <- clippingParser
  return $ LabeledClipping (Just label) clipping

lineLabeledClippingsParser :: Parser [LabeledClipping]
lineLabeledClippingsParser = sepByWhitespaces labeledClippingParser

extendedRectangle :: Int -> Rectangle -> Rectangle
extendedRectangle margin (Rectangle (Point x0 y0) (Point x1 y1)) =
  Rectangle (Point (x0 `nonNegativeDiff` margin) (y0 `nonNegativeDiff` margin))
            (Point (x1 + margin) (y1 + margin))

smallerRectangle :: Int -> Rectangle -> Rectangle
smallerRectangle margin (Rectangle (Point x0 y0) (Point x1 y1)) =
  Rectangle (Point (x0 + margin) (y0 + margin))
            (Point (x1 `nonNegativeDiff` margin) (y1 `nonNegativeDiff` margin))



nonNegativeDiff x y
  | x < y = 0
  | otherwise = x - y

rectangleParser :: Parser Rectangle
rectangleParser = do
  x0 <- decimal
  char ','
  y0 <- decimal
  char ','
  x1 <- decimal
  char ','
  y1 <- decimal
  return $ Rectangle (Point x0 y0) (Point x1 y1)

rectangleArea :: Rectangle -> Integer
rectangleArea (Rectangle (Point x0 y0) (Point x1 y1)) =
  (fromIntegral $ x1 - x0) * (fromIntegral $ y1 - y0)

clippingArea :: LabeledClipping -> Integer
clippingArea (LabeledClipping _ (Clipping _ rect)) = rectangleArea rect

totalArea :: [LabeledClipping] -> Integer
totalArea = sum . Prelude.map clippingArea

coveredBy :: [LabeledClipping] -> [LabeledClipping] -> Integer
coveredBy clippingsA clippingsB = sum
                                  $ Prelude.map rectangleArea
                                  $ catMaybes
                                  $ Data.List.unfoldr step (clippingsA, clippingsB)
  where
    step ([], _) = Nothing
    step (firstA:restA, b) = Just (result, (newA ++ restA, newB))
       where (result, newA, newB) = step' firstA b
    step' rectA [] = (Nothing, [], [])
    step' a (firstB:restB) = case partitionClippings a firstB of
      Just (commonRect, leftoversA, leftoversB) -> (Just commonRect, leftoversA, leftoversB ++ restB)
      Nothing -> let
                (result, leftoversA, leftoversB) = step' a restB
                in (result, leftoversA, firstB:leftoversB)

partitionClippings :: LabeledClipping -> LabeledClipping -> Maybe (Rectangle, [LabeledClipping], [LabeledClipping])
partitionClippings (LabeledClipping label (Clipping page rect@(Rectangle (Point x0 y0) (Point x1 y1))))
                   (LabeledClipping label' (Clipping page' rect'@(Rectangle (Point x0' y0') (Point x1' y1'))))
  | label == label' && page == page' && not (areDisjoint rect rect') = Just (commonRect, leftovers, leftovers')
  | otherwise = Nothing
  where commonRect = Rectangle (Point cx0 cy0) (Point cx1 cy1)
        cx0 = max x0 x0'
        cx1 = min x1 x1'
        cy0 = max y0 y0'
        cy1 = min y1 y1'
        leftovers = Prelude.map (\r -> LabeledClipping label (Clipping page r)) $ getLeftovers commonRect rect
        leftovers' = Prelude.map (\r -> LabeledClipping label (Clipping page r)) $ getLeftovers commonRect rect'

areDisjoint :: Rectangle -> Rectangle -> Bool
areDisjoint (Rectangle (Point x0 y0) (Point x1 y1))
            (Rectangle (Point x0' y0') (Point x1' y1')) =
  x1 < x0' || x1' < x0 || y1 < y0' || y1' < y0

getLeftovers :: Rectangle -> Rectangle -> [Rectangle]
getLeftovers (Rectangle (Point x0 y0) (Point x1 y1))
             (Rectangle (Point x0' y0') (Point x1' y1')) =
  Prelude.filter validRectangle [Rectangle (Point x0 y0') (Point x1 (y0 - 1)),
                         Rectangle (Point x0 (y1 + 1)) (Point x1 y1'),
                         Rectangle (Point x0' y0') (Point (x0 - 1) y1'),
                         Rectangle (Point (x1 + 1) y0') (Point x1' y1')]
  where validRectangle (Rectangle (Point x0 y0) (Point x1 y1)) = x0 <= x1 && y0 <= y1
