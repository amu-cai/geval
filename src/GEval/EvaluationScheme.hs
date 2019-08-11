module GEval.EvaluationScheme
  (EvaluationScheme(..), evaluationSchemeMetric, applyPreprocessingOperations, evaluationSchemeName)
  where

import GEval.Metric

import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light.Base (Regex(..))
import Data.Text (Text(..), concat, toLower, toUpper, pack, unpack)
import Data.List (intercalate, break)
import Data.Either
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.UTF8 as BSU


data EvaluationScheme = EvaluationScheme Metric [PreprocessingOperation]
  deriving (Eq)

data PreprocessingOperation = RegexpMatch Regex | LowerCasing | UpperCasing | SetName Text
  deriving (Eq)

leftParameterBracket :: Char
leftParameterBracket = '<'

rightParameterBracket :: Char
rightParameterBracket = '>'

instance Read EvaluationScheme where
  readsPrec _ s = [(EvaluationScheme metric ops, theRest)]
    where (metricS, opS) = break (== ':') s
          metric = read metricS
          (ops, theRest) = case opS of
            "" -> ([], "")
            (_:opS') -> readOps opS'

readOps :: String -> ([PreprocessingOperation], String)
readOps ('l':theRest) = (LowerCasing:ops, theRest')
    where (ops, theRest') = readOps theRest
readOps ('u':theRest) = (UpperCasing:ops, theRest')
    where (ops, theRest') = readOps theRest
readOps ('m':theRest) = handleParametrizedOp (RegexpMatch . (fromRight undefined) . ((flip compileM) []) . BSU.fromString) theRest
readOps ('N':theRest) = handleParametrizedOp (SetName . pack) theRest
readOps s = ([], s)

handleParametrizedOp :: (String -> PreprocessingOperation) -> String -> ([PreprocessingOperation], String)
handleParametrizedOp constructor (leftParameterBracket:theRest) =
  case break (== rightParameterBracket) theRest of
    (s, []) -> ([], s)
    (param, (_:theRest')) -> let (ops, theRest'') = readOps theRest'
                            in ((constructor param):ops, theRest'')
handleParametrizedOp _ s = ([], s)

instance Show EvaluationScheme where
  show (EvaluationScheme metric operations) = (show metric) ++ (if null operations
                                                                then ""
                                                                else ":" ++ (Prelude.concat (map show operations)))

evaluationSchemeName :: EvaluationScheme -> String
evaluationSchemeName scheme@(EvaluationScheme metric operations) = fromMaybe (show scheme) (findNameSet operations)

findNameSet :: [PreprocessingOperation] -> Maybe String
findNameSet [] = Nothing
findNameSet ((SetName name):_) = Just (unpack name)
findNameSet (_:ops) = findNameSet ops

evaluationSchemeMetric :: EvaluationScheme -> Metric
evaluationSchemeMetric (EvaluationScheme metric _) = metric

instance Show PreprocessingOperation where
  show (RegexpMatch (Regex _ regexp)) = parametrizedOperation "m" (BSU.toString regexp)
  show LowerCasing = "l"
  show UpperCasing = "u"
  show (SetName t) = parametrizedOperation "N" (unpack t)

parametrizedOperation :: String -> String -> String
parametrizedOperation opCode opArg = opCode ++ [leftParameterBracket] ++ opArg ++ [rightParameterBracket]

applyPreprocessingOperations :: EvaluationScheme -> Text -> Text
applyPreprocessingOperations (EvaluationScheme _ operations) t = foldl (flip applyPreprocessingOperation) t operations

applyPreprocessingOperation :: PreprocessingOperation -> Text -> Text
applyPreprocessingOperation (RegexpMatch regex) = Data.Text.concat . (map fst) . (scan regex)
applyPreprocessingOperation LowerCasing = toLower
applyPreprocessingOperation UpperCasing = toUpper
applyPreprocessingOperation (SetName _) = id
