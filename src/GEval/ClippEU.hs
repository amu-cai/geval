
module GEval.ClippEU
       where

import Data.Attoparsec.Text
import Data.Text
import Control.Applicative
import Control.Exception

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

sepByWhitespaces :: Parser a -> Parser [a]
sepByWhitespaces parser = possibleWhitespace *> parser `sepBy` whitespace <* possibleWhitespace <* endOfInput

clippingSpecParser :: Parser ClippingSpec
clippingSpecParser = do
  pageNo <- PageNumber <$> decimal
  char '/'
  rectangle <- rectangleParser
  char '/'
  margin <- decimal
  return $ ClippingSpec pageNo (smallerRectangle margin rectangle) (extendedRectangle margin rectangle)

possibleWhitespace = many' (satisfy isHorizontalSpace)

whitespace = many1 (satisfy isHorizontalSpace)

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
