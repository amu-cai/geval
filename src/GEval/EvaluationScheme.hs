module GEval.EvaluationScheme
  (EvaluationScheme(..),
   evaluationSchemeMetric,
   applyPreprocessingOperations,
   evaluationSchemeName,
   evaluationSchemePriority,
   getRegexpMatch,
   PreprocessingOperation(..))
  where

import GEval.Metric

import Debug.Trace

import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light.Base (Regex(..))
import Text.Regex.PCRE.Light (compile)
import Data.Text (Text(..), concat, toCaseFold, toLower, toUpper, pack, unpack, words, unwords)
import Data.List (intercalate, break, sort)
import Data.Either
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.ByteString.UTF8 as BSU


data EvaluationScheme = EvaluationScheme Metric [PreprocessingOperation]
  deriving (Eq)

data PreprocessingOperation = RegexpMatch Regex
                              | RegexpTokenMatch Regex
                              | LowerCasing
                              | UpperCasing
                              | CaseFolding
                              | Sorting
                              | SetName Text
                              | SetPriority Int
                              | RegexpSubstition Regex Text
                              | FeatureFilter Text
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
readOps ('c':theRest) = (CaseFolding:ops, theRest')
    where (ops, theRest') = readOps theRest
readOps ('m':theRest) = handleParametrizedOp (RegexpMatch . ((flip compile) []) . BSU.fromString) theRest
readOps ('t':theRest) = handleParametrizedOp (RegexpTokenMatch . ((flip compile) []) . BSU.fromString) theRest
readOps ('S':theRest) = (Sorting:ops, theRest')
    where (ops, theRest') = readOps theRest
readOps ('N':theRest) = handleParametrizedOp (SetName . pack) theRest
readOps ('P':theRest) = handleParametrizedOp (SetPriority . read) theRest
readOps ('s':theRest) = handleParametrizedBinaryOp (\a b -> RegexpSubstition (compile (BSU.fromString a) []) (pack b)) theRest
readOps ('f':theRest) = handleParametrizedOp (FeatureFilter . pack) theRest
readOps s = ([], s)

getRegexpMatch :: EvaluationScheme -> Maybe Regex
getRegexpMatch (EvaluationScheme _ ops) = getRegexpMatch' ops
  where getRegexpMatch' [] = Nothing
        getRegexpMatch' ((RegexpMatch regex):_) = Just regex
        getRegexpMatch' (_:ops) = getRegexpMatch' ops

handleParametrizedOp :: (String -> PreprocessingOperation) -> String -> ([PreprocessingOperation], String)
handleParametrizedOp constructor theRest =
  case parseParameter theRest of
    (Nothing, s) -> ([], s)
    (Just param, theRest') -> let (ops, theRest'') = readOps theRest'
                             in ((constructor param):ops, theRest'')

handleParametrizedBinaryOp :: (String -> String -> PreprocessingOperation) -> String -> ([PreprocessingOperation], String)
handleParametrizedBinaryOp constructor theRest =
  case parseParameter theRest of
    (Nothing, s) -> ([], s)
    (Just paramA, theRest') ->
      case parseParameter theRest' of
        (Nothing, s) -> ([], s)
        (Just paramB, theRest'') -> let (ops, theRest''') = readOps theRest''
                                   in ((constructor paramA paramB):ops, theRest''')

parseParameter :: String -> (Maybe String, String)
parseParameter (leftParameterBracket:theRest) =
  case break (== rightParameterBracket) theRest of
    (s, []) -> (Nothing, s)
    (param, (_:theRest')) -> (Just param, theRest')
parseParameter s = (Nothing, s)


instance Show EvaluationScheme where
  show (EvaluationScheme metric operations) = (show metric) ++ (if null operations
                                                                then ""
                                                                else ":" ++ (Prelude.concat (map show operations)))

evaluationSchemeName :: EvaluationScheme -> String
evaluationSchemeName scheme@(EvaluationScheme metric operations) = fromMaybe (show scheme) (findNameSet operations)

evaluationSchemePriority scheme@(EvaluationScheme _ operations) = fromMaybe defaultPriority (findPrioritySet operations)
  where defaultPriority = 1

findNameSet :: [PreprocessingOperation] -> Maybe String
findNameSet ops = case names of
  [] -> Nothing
  _ -> Just $ intercalate " " names
  where names = catMaybes $ map extractName ops
        extractName (SetName n) = Just (unpack n)
        extractName _ = Nothing

findPrioritySet :: [PreprocessingOperation] -> Maybe Int
findPrioritySet [] = Nothing
findPrioritySet ((SetPriority p):_) = Just p
findPrioritySet (_:ops) = findPrioritySet ops

evaluationSchemeMetric :: EvaluationScheme -> Metric
evaluationSchemeMetric (EvaluationScheme metric _) = metric

instance Show PreprocessingOperation where
  show (RegexpMatch (Regex _ regexp)) = parametrizedOperation "m" (BSU.toString regexp)
  show (RegexpTokenMatch (Regex _ regexp)) = parametrizedOperation "t" (BSU.toString regexp)
  show LowerCasing = "l"
  show UpperCasing = "u"
  show CaseFolding = "c"
  show Sorting = "S"
  show (SetName t) = parametrizedOperation "N" (unpack t)
  show (SetPriority p) = parametrizedOperation "P" (show p)
  show (RegexpSubstition (Regex _ regexp) s) = "s" ++ (formatParameter $ BSU.toString regexp) ++ (formatParameter $ unpack s)
  show (FeatureFilter featureSpec) = parametrizedOperation "f" (unpack featureSpec)

applySubstitution :: Regex -> Text -> Text -> Text
applySubstitution r substitution t =
  gsub r (handleRefs substitution) t

handleRefs :: Text -> Text -> [Text] -> Text
handleRefs substitution mainMatch subMatches = gsub refRegexp handleRef substitution
  where refRegexp = compile (BSU.fromString "\\\\\\d+") []
        indexables = mainMatch : subMatches
        handleRef :: Text -> Text
        handleRef ref =
          let ix = (read $ tail $ unpack ref)
          in if ix >= length indexables
             then (pack "")
             else indexables !! ix

parametrizedOperation :: String -> String -> String
parametrizedOperation opCode opArg = opCode ++ (formatParameter opArg)

formatParameter :: String -> String
formatParameter p = [leftParameterBracket] ++ p ++ [rightParameterBracket]

applyPreprocessingOperations :: EvaluationScheme -> Text -> Text
applyPreprocessingOperations (EvaluationScheme _ operations) t = foldl (flip applyPreprocessingOperation) t operations

applyPreprocessingOperation :: PreprocessingOperation -> Text -> Text
applyPreprocessingOperation (RegexpMatch regex) = Data.Text.concat . (map fst) . (scan regex)
applyPreprocessingOperation (RegexpTokenMatch regex) = Data.Text.unwords . (filter (≈ regex)) . Data.Text.words
applyPreprocessingOperation LowerCasing = toLower
applyPreprocessingOperation UpperCasing = toUpper
applyPreprocessingOperation CaseFolding = toCaseFold
applyPreprocessingOperation Sorting = Data.Text.unwords . sort . Data.Text.words
applyPreprocessingOperation (SetName _) = id
applyPreprocessingOperation (SetPriority _) = id
applyPreprocessingOperation (RegexpSubstition regex substition) = applySubstitution regex substition
applyPreprocessingOperation (FeatureFilter _) = id
