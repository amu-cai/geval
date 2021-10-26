module GEval.Common
       where

import qualified Data.Text as T
import Data.Text.Read as TR

import Data.Word
import Control.Exception

import Data.Attoparsec.Text hiding (Done)

data SourceItem a = Got a | Wrong String | Done

instance Functor SourceItem where
  fmap f (Got a) = Got (f a)
  fmap _ (Wrong s) = Wrong s
  fmap _ Done = Done

type MetricValue = Double

data GraphSeries = GraphSeries [(Double, Double)]

data FormattingOptions = FormattingOptions {
  decimalPlaces :: Maybe Int,
  asPercentage :: Bool
  }
  deriving (Show)

data MetricResult = SimpleRun MetricValue | BootstrapResampling [MetricValue]

instance Show MetricResult where
  show (SimpleRun val) = show val
  show (BootstrapResampling vals) = show vals

data MetricOutput = MetricOutput MetricResult (Maybe GraphSeries)

getMetricValue :: MetricOutput -> MetricResult
getMetricValue (MetricOutput v _) = v

extractSimpleRunValue :: MetricResult -> MetricValue
extractSimpleRunValue (SimpleRun v) = v
extractSimpleRunValue (BootstrapResampling (v:_)) = v

getGraphSeries :: MetricOutput -> Maybe GraphSeries
getGraphSeries (MetricOutput _ gs) = gs

-- some operations can be "hard" (on ints) or "soft" (on doubles),
-- introduce a typeclass so that we could generalise easily
class ConvertibleToDouble n where
  toDouble :: n -> Double

instance ConvertibleToDouble Double where
  toDouble = id

instance ConvertibleToDouble Int where
  toDouble = fromIntegral

instance ConvertibleToDouble Integer where
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

bigrams :: [a] -> [(a, a)]
bigrams [] = []
bigrams [_] = []
bigrams u = zip u $ tail u

class AEq a where
    (=~) :: a -> a -> Bool

instance AEq Double where
    x =~ y = abs ( x - y ) < (1.0e-4 :: Double)

itemAbsoluteError :: (Double, Double) -> Double
itemAbsoluteError (exp, out) = abs (exp-out)

itemSquaredError :: (Double, Double) -> Double
itemSquaredError (exp, out) = (exp-out)**2

itemLogLossError :: (Double, Double) -> Double
itemLogLossError (exp, out)
  | exp' > 0.5 = - (log out')
  | otherwise = - (log (1 - out'))
  where exp' = normalizeAsProb exp
        out' = normalizeAsProb out
        normalizeAsProb v
          | v >= 1.0 = 1.0
          | v <= 0.0 = 0.0
          | otherwise = v

data GEvalException = NoExpectedFile FilePath
                      | NoOutFile FilePath
                      | NoExpectedDirectory FilePath
                      | NoOutDirectory FilePath
                      | NoExpectedTestDirectory FilePath
                      | NoOutTestDirectory FilePath
                      | NoInputFile FilePath
                      | FileAlreadyThere FilePath
                      | TooFewLines
                      | TooManyLines
                      | TooFewLinesInInput
                      | TooManyLinesInInput
                      | EmptyOutput
                      | UnexpectedData Word32 String
                      | UnexpectedMultipleOutputs
                      | OtherException String
                      | NoHeaderFile FilePath
                      | UnknownMetric String
                      | UnknownFlags String
                      deriving (Eq)

instance Exception GEvalException

instance Show GEvalException where
  show (NoExpectedFile filePath) = somethingWrongWithFilesMessage "No file with the expected results" filePath
  show (NoOutFile filePath) = somethingWrongWithFilesMessage "No file with the test results" filePath
  show (NoExpectedDirectory filePath) = somethingWrongWithFilesMessage "No directory with the expected results" filePath
  show (NoOutDirectory filePath) = somethingWrongWithFilesMessage "No directory with the test results" filePath
  show (NoExpectedTestDirectory filePath) = somethingWrongWithFilesMessage "No test subdirectory with the expected results" filePath
  show (NoOutTestDirectory filePath) = somethingWrongWithFilesMessage "No test subdirectory with the results obtained" filePath
  show (NoInputFile filePath) = somethingWrongWithFilesMessage "No file with the input" filePath
  show (FileAlreadyThere filePath) = somethingWrongWithFilesMessage "File already there" filePath
  show TooFewLines = "Too few lines in the output file"
  show TooManyLines = "Too many lines in the output file"
  show TooFewLinesInInput = "Too few lines in the input file"
  show TooManyLinesInInput = "Too many lines in the input file"
  show EmptyOutput = "The output file is empty"
  show (UnexpectedData lineNo message) = "Line " ++ (show lineNo) ++ ": Unexpected data [" ++ message ++ "]"
  show UnexpectedMultipleOutputs = "Multiple outputs are not possible in this mode, use -o option to select an output file"
  show (OtherException message) = message
  show (NoHeaderFile filePath) = somethingWrongWithFilesMessage "No file with header specification" filePath
  show (UnknownMetric t) = "Unknown or broken metric definition: " ++ t
  show (UnknownFlags t) = "Unknown or broken metric flags: " ++ t

somethingWrongWithFilesMessage :: String -> FilePath -> String
somethingWrongWithFilesMessage msg filePath = Prelude.concat
                                [ msg, ": `", filePath, "`" ]
