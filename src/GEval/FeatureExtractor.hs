{-# LANGUAGE OverloadedStrings #-}

module GEval.FeatureExtractor
  (extractFactors,
   extractFactorsFromTabbed,
   cartesianFeatures,
   LineWithFactors(..),
   LineWithPeggedFactors(..),
   PeggedFactor(..),
   PeggedExistentialFactor(..),
   Factor(..),
   SimpleFactor(..),
   ExistentialFactor(..),
   AtomicFactor(..),
   FeatureNamespace(..),
   filterExistentialFactors)
  where

import Data.Text
import Data.List
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Text.Tokenizer
import Text.WordShape
import GEval.BlackBoxDebugging
import GEval.Common
import Text.Read (readMaybe)

data LineWithFactors = LineWithFactors Double MetricValue [Factor]
                              deriving (Eq, Ord)

data Factor = UnaryFactor PeggedFactor | CartesianFactor PeggedExistentialFactor PeggedExistentialFactor
               deriving (Eq, Ord)

instance Show Factor where
  show (UnaryFactor factor) = show factor
  show (CartesianFactor factorA factorB) = (show factorA) ++ "~~" ++ (show factorB)

data LineWithPeggedFactors = LineWithPeggedFactors Double MetricValue [PeggedFactor]
                              deriving (Eq, Ord)

data PeggedFactor = PeggedFactor FeatureNamespace SimpleFactor
               deriving (Eq, Ord)

instance Show PeggedFactor where
  show (PeggedFactor namespace factor) = (show namespace) ++ ":" ++ (show factor)

data PeggedExistentialFactor = PeggedExistentialFactor FeatureNamespace ExistentialFactor
                               deriving (Eq, Ord)

instance Show PeggedExistentialFactor where
  show (PeggedExistentialFactor namespace factor) = (show namespace) ++ ":" ++ (show factor)

data SimpleFactor = SimpleExistentialFactor ExistentialFactor | NumericalFactor (Maybe Double) Int
               deriving (Eq, Ord)

instance Show SimpleFactor where
  show (SimpleExistentialFactor factor) = show factor
  show (NumericalFactor (Just v) _) = ("=" ++ (show v))
  show (NumericalFactor (Nothing) l) = ("=#" ++ (show l))

data ExistentialFactor = SimpleAtomicFactor AtomicFactor | BigramFactor AtomicFactor AtomicFactor
                         deriving (Eq, Ord)

instance Show ExistentialFactor where
  show (SimpleAtomicFactor factor) = show factor
  show (BigramFactor factorA factorB) = (show factorA) ++ "++" ++ (show factorB)

data AtomicFactor = TextFactor Text | ShapeFactor WordShape
                     deriving (Eq, Ord)

instance Show AtomicFactor where
  show (TextFactor t) = unpack t
  show (ShapeFactor (WordShape t)) = 'S':'H':'A':'P':'E':':':(unpack t)

data FeatureNamespace = FeatureNamespace Text | FeatureTabbedNamespace Text Int
                        deriving (Eq, Ord)

instance Show FeatureNamespace where
  show (FeatureNamespace namespace) = unpack namespace
  show (FeatureTabbedNamespace namespace column) = ((unpack namespace) ++ "<" ++ (show column) ++ ">")

tokenizeForFactors :: (Maybe Tokenizer) -> Text -> [Text]
tokenizeForFactors Nothing t = Data.List.filter (not . Data.Text.null) $ split splitPred t
   where splitPred c = c == ' ' || c == '\t' || c == ':'
tokenizeForFactors mTokenizer t = tokenize mTokenizer t

extractAtomicFactors :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> [[AtomicFactor]]
extractAtomicFactors mTokenizer bbdo t = [Data.List.map TextFactor tokens] ++
  (if bbdoWordShapes bbdo
    then [nub $ Data.List.map (ShapeFactor . shapify) tokens]
    else [])
  where tokens = nub $ (tokenizeForFactors mTokenizer) t

extractSimpleFactors :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> [SimpleFactor]
extractSimpleFactors mTokenizer bbdo t = Data.List.concat $ (Prelude.map (Prelude.map SimpleExistentialFactor) existentials) ++
                                                            (if bbdoConsiderNumericalFeatures bbdo
                                                             then [numericalFactor t]
                                                             else [])
  where atomss = extractAtomicFactors mTokenizer bbdo t
        existentials = (Prelude.map (Prelude.map SimpleAtomicFactor) atomss) ++
                       (if bbdoBigrams bbdo
                        then Prelude.map bigramFactors atomss
                        else [])
        bigramFactors atoms = Prelude.map (\(a, b) -> BigramFactor a b) $ bigrams atoms
        numericalFactor t = [NumericalFactor (readMaybe $ unpack t) (Data.Text.length t)]
extractFactors :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> Text -> [PeggedFactor]
extractFactors mTokenizer bbdo namespace record =
  Prelude.map (\af -> PeggedFactor (FeatureNamespace namespace) af)
  $ extractSimpleFactors mTokenizer bbdo record

extractFactorsFromTabbed :: (Maybe Tokenizer) -> BlackBoxDebuggingOptions -> Text -> Text -> [PeggedFactor]
extractFactorsFromTabbed mTokenizer bbdo namespace record =
  Data.List.concat
  $ Prelude.map (\(n, t) -> Prelude.map (\af -> PeggedFactor (FeatureTabbedNamespace namespace n) af) $ extractSimpleFactors mTokenizer bbdo t)
  $ Prelude.zip [1..] (splitOn "\t" record)

addCartesianFactors :: BlackBoxDebuggingOptions -> [LineWithPeggedFactors] -> [LineWithFactors]
addCartesianFactors bbdo linesWithPeggedFactors = addCartesianFactors' (bbdoCartesian bbdo) linesWithPeggedFactors
  where addCartesianFactors' _ linesWithPeggedFactors
          = Prelude.map (\(LineWithPeggedFactors rank score fs) ->
                            LineWithFactors rank score (Prelude.map UnaryFactor fs)) linesWithPeggedFactors

cartesianFeatures :: [PeggedExistentialFactor] -> [Factor]
cartesianFeatures factors = nub $ [CartesianFactor a b | a <- factors, b <- factors, a < b]

filterExistentialFactors :: [PeggedFactor] -> [PeggedExistentialFactor]
filterExistentialFactors factors = catMaybes $ Prelude.map toExistential factors
  where toExistential (PeggedFactor namespace (SimpleExistentialFactor factor)) = Just $ PeggedExistentialFactor namespace factor
        toExistential _ = Nothing
