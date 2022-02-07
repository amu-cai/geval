{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GEval.Clippings
       where

import Data.Attoparsec.Text
import Data.Text
import Data.Char (isSpace)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)

import GEval.Common
import GEval.PrecisionRecall (maxMatch)
import GEval.Probability

newtype PageNumber = PageNumber Int
        deriving (Eq)

instance Show PageNumber where
  show (PageNumber p) = show p

data Point = Point Int Int
             deriving (Eq)

instance Show Point where
  show (Point x y) = (show x) ++ "," ++ (show y)

data Rectangle = Rectangle Point Point
                 deriving (Eq)

instance Show Rectangle where
  show (Rectangle a b) = (show a) ++ "," ++ (show b)

-- page number is optional (Nothing if one-page documents are assumed)
data Clipping = Clipping (Maybe PageNumber) Rectangle
                deriving (Eq)

instance Show Clipping where
  show (Clipping Nothing rect) = show rect
  show (Clipping (Just pageNumber) rect) = (show pageNumber) ++ "/" ++ (show rect)

data ClippingSpec = ClippingSpec (Maybe PageNumber) Rectangle Rectangle
                    deriving (Eq, Show)

data LabeledClipping = LabeledClipping (Maybe Text) Clipping
                       deriving (Eq)

instance Show LabeledClipping where
  show (LabeledClipping Nothing clipping) = show clipping
  show (LabeledClipping (Just label) clipping) = (unpack label) ++ ":" ++ (show clipping)

getRectangle :: LabeledClipping -> Rectangle
getRectangle (LabeledClipping _ (Clipping _ rect)) = rect

getLabel :: LabeledClipping -> Maybe Text
getLabel (LabeledClipping label _) = label

data ObtainedLabeledClipping = ObtainedLabeledClipping LabeledClipping Probability
                               deriving (Eq, Show)

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
  choice [clippingParserWithPage,
          clippingParserWithoutPage]

clippingParserWithPage :: Parser Clipping
clippingParserWithPage = do
  pageNo <- PageNumber <$> decimal
  _ <- char '/'
  rectangle <- rectangleParser
  return $ Clipping (Just pageNo) rectangle

clippingParserWithoutPage :: Parser Clipping
clippingParserWithoutPage = do
  rectangle <- rectangleParser
  return $ Clipping Nothing rectangle


lineClippingSpecsParser :: Parser [ClippingSpec]
lineClippingSpecsParser = sepByWhitespaces clippingSpecParser

clippingSpecParser :: Parser ClippingSpec
clippingSpecParser = do
  choice [clippingSpecParserWithPage,
          clippingSpecParserWithoutPage]

clippingSpecParserWithPage :: Parser ClippingSpec
clippingSpecParserWithPage = do
  pageNo <- PageNumber <$> decimal
  _ <- char '/'
  rectangle <- rectangleParser
  _ <- char '/'
  margin <- decimal
  return $ ClippingSpec (Just pageNo) (smallerRectangle margin rectangle) (extendedRectangle margin rectangle)

clippingSpecParserWithoutPage :: Parser ClippingSpec
clippingSpecParserWithoutPage = do
  rectangle <- rectangleParser
  _ <- char '/'
  margin <- decimal
  return $ ClippingSpec Nothing (smallerRectangle margin rectangle) (extendedRectangle margin rectangle)


labeledClippingParser :: Parser LabeledClipping
labeledClippingParser =
  choice [clippingWithLabelParser,
          (LabeledClipping Nothing <$> clippingParser)]

clippingWithLabelParser :: Parser LabeledClipping
clippingWithLabelParser = do
  label <- takeWhile1 (\c -> not (isSpace c) && c /= ':')
  _ <- string ":"
  clipping <- clippingParser
  return $ LabeledClipping (Just label) clipping

obtainedLabeledClippingParser :: Parser ObtainedLabeledClipping
obtainedLabeledClippingParser = do
  labeledClipping <- labeledClippingParser
  prob <- option probabilityOne ((string ":") *> (mkProbability <$> double))
  return $ ObtainedLabeledClipping labeledClipping prob

lineLabeledClippingsParser :: Parser [LabeledClipping]
lineLabeledClippingsParser = sepByWhitespaces labeledClippingParser

lineObtainedLabeledClippingsParser :: Parser [ObtainedLabeledClipping]
lineObtainedLabeledClippingsParser = sepByWhitespaces obtainedLabeledClippingParser

extendedRectangle :: Int -> Rectangle -> Rectangle
extendedRectangle margin (Rectangle (Point x0 y0) (Point x1 y1)) =
  Rectangle (Point (x0 `nonNegativeDiff` margin) (y0 `nonNegativeDiff` margin))
            (Point (x1 + margin) (y1 + margin))

smallerRectangle :: Int -> Rectangle -> Rectangle
smallerRectangle margin (Rectangle (Point x0 y0) (Point x1 y1)) =
  Rectangle (Point (x0 + margin) (y0 + margin))
            (Point (x1 `nonNegativeDiff` margin) (y1 `nonNegativeDiff` margin))

nonNegativeDiff :: (Ord p, Num p) => p -> p -> p
nonNegativeDiff x y
  | x < y = 0
  | otherwise = x - y

rectangleParser :: Parser Rectangle
rectangleParser = do
  x0 <- decimal
  _ <- char ','
  y0 <- decimal
  _ <- char ','
  x1 <- decimal
  _ <- char ','
  y1 <- decimal
  if x1 < x0 || y1 < y0
  then fail "wrong coordinates"
  else return $ Rectangle (Point x0 y0) (Point x1 y1)

rectangleArea :: Rectangle -> Integer
rectangleArea (Rectangle (Point x0 y0) (Point x1 y1)) =
  (fromIntegral $ x1 - x0 + 1) * (fromIntegral $ y1 - y0 + 1)

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
    step' _ [] = (Nothing, [], [])
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
  where validRectangle (Rectangle (Point x0'' y0'') (Point x1'' y1'')) = x0'' <= x1'' && y0'' <= y1''

clippEUMatchStep :: ([ClippingSpec], [Clipping]) -> (Int, Int, Int)
clippEUMatchStep (clippingSpecs, clippings) = (maxMatch matchClippingToSpec clippingSpecs clippings,
                                               Prelude.length clippingSpecs,
                                               Prelude.length clippings)

instance Coverable LabeledClipping where
  coveredScore clipA clipB = intersectionArea /. areaOfA
    where areaOfA = clippingArea clipA
          intersectionArea = coveredBy [clipA] [clipB]
  disjoint clipA clipB = areDisjoint (getRectangle clipA) (getRectangle clipB)

instance EntityWithProbability ObtainedLabeledClipping where
  type BareEntity ObtainedLabeledClipping = LabeledClipping
  getBareEntity (ObtainedLabeledClipping clipping _) = clipping
  getProbabilityAsDouble (ObtainedLabeledClipping _ prob) = getP prob
  matchScore clipA oclipB
    | labelA == labelB = intersectionArea /. unionArea
    | otherwise = 0.0
    where clipB = getBareEntity oclipB
          labelA = getLabel clipA
          labelB = getLabel clipB
          intersectionArea = coveredBy [clipA] [clipB]
          unionArea = (clippingArea clipA) + (clippingArea clipB) - intersectionArea
