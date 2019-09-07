{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GEval.ProbList
       (parseIntoProbList, selectByStandardThreshold, countLogLossOnProbList, ProbList(..))
       where

import qualified Data.Text as T

import GEval.Common
import GEval.Probability


data ProbList = ProbList [WordWithProb]
  deriving (Show)

data WordWithProb = WordWithProb T.Text Probability
  deriving (Show)

instance EntityWithProbability WordWithProb where
  type BareEntity WordWithProb = T.Text
  getBareEntity (WordWithProb w _) = w
  getProbability (WordWithProb _ p) = p
  matchScore w1 (WordWithProb w2 _)
    | w1 == w2 = 1.0
    | otherwise = 0.0

parseIntoWordWithProb :: T.Text -> WordWithProb
parseIntoWordWithProb t =
  -- If anything is wrong the whole word is treated as a label,
  -- even if it contains colons, digits, dots, etc.
  if T.null wordSpecPart
  then wordWithoutProb
  else
    if "." `T.isInfixOf` numberPart
    then case textToDouble numberPart of
       Right p -> if isProbability p
                   then WordWithProb wordSpecPart (mkProbability p)
                   else wordWithoutProb
       Left _ -> wordWithoutProb
    else wordWithoutProb
  where
    (wordSpecPart', numberPart) = T.breakOnEnd ":" t
    wordWithoutProb = WordWithProb t probabilityOne
    wordSpecPart = if T.null wordSpecPart'
                   then wordSpecPart'
                   else T.init wordSpecPart'

parseIntoProbList :: T.Text -> ProbList
parseIntoProbList = ProbList . map parseIntoWordWithProb . T.words

selectByThreshold :: Probability -> ProbList -> [T.Text]
selectByThreshold threshold (ProbList l) =
  map (\(WordWithProb w _) -> w)
  $ filter (\(WordWithProb _ p) -> p >= threshold) l

standardThreshold :: Double
standardThreshold = 0.5

selectByStandardThreshold :: ProbList -> [T.Text]
selectByStandardThreshold = selectByThreshold (mkProbability standardThreshold)

findProb :: ProbList -> T.Text -> Probability
findProb (ProbList probList) target =
  case filter (\(WordWithProb w _) -> w == target) probList of
    ((WordWithProb _ p):_) -> p
    [] -> probabilityZero

countLogLossOnProbList :: [T.Text] -> ProbList -> Double
countLogLossOnProbList expected probList@(ProbList l)  =
  - (logLossForCorrectOnes + logLossForIncorrectOnes)
  where logLossForCorrectOnes =
          sum  $ map (\ew -> log ( getP (findProb probList ew))) expected
        logLossForIncorrectOnes =
          sum
          $ map (\(WordWithProb _ p) -> log (1.0 - getP p))
          $ filter (\(WordWithProb w p) -> w `notElem` expected) l
