module GEval.Common
       where

import qualified Data.Text as T
import Data.Text.Read as TR

import Data.Attoparsec.Text

-- some operations can be "hard" (on ints) or "soft" (on doubles),
-- introduce a typeclass so that we could generalise easily
class ConvertibleToDouble n where
  toDouble :: n -> Double

instance ConvertibleToDouble Double where
  toDouble = id

instance ConvertibleToDouble Int where
  toDouble = fromIntegral

(/.) :: (ConvertibleToDouble f, Integral a) => f -> a -> Double
x /. 0 = 1.0
x /. y = (toDouble x) / (fromIntegral y)

safeDoubleDiv :: Double -> Double -> Double
safeDoubleDiv _ 0.0 = 0.0
safeDoubleDiv x y = x / y

log2 :: Double -> Double
log2 x = (log x) / (log 2.0)

entropyWithTotalGiven total distribution = - (sum $ map (entropyCount total) distribution)

entropyCount :: Int -> Int -> Double
entropyCount total count = prob * (log2 prob)
  where prob = count /. total

textToDouble :: T.Text -> Either String Double
textToDouble t = case TR.double t of
  Right (x, reminder) -> if T.null reminder
                        then
                          Right x
                        else
                          Left "number text found after a number"
  Left m -> Left m

sepByWhitespaces :: Parser a -> Parser [a]
sepByWhitespaces parser = possibleWhitespace *> parser `sepBy` whitespace <* possibleWhitespace <* endOfInput

possibleWhitespace = many' (satisfy isHorizontalSpace)

whitespace = many1 (satisfy isHorizontalSpace)

indicator :: Bool -> Double
indicator True = 1.0
indicator False = 0.0
