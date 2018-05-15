{-# LANGUAGE OverloadedStrings #-}

module GEval.BIO
       (BIOLabel(..), bioSequenceParser, parseBioSequenceIntoEntities, TaggedSpan(..), TaggedEntity(..), gatherCountsForBIO)
       where

import GEval.PrecisionRecall

import qualified Data.Text as T

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Char
import Data.Maybe (catMaybes)

import GEval.Common

data BIOLabel = Outside | Beginning T.Text (Maybe T.Text) | Inside T.Text (Maybe T.Text)
                deriving (Eq, Show)

data TaggedSpan = TaggedSpan Int Int
                  deriving (Eq, Show)

data TaggedEntity = TaggedEntity TaggedSpan T.Text (Maybe T.Text)
                    deriving (Eq, Show)

gatherCountsForBIO :: [TaggedEntity] -> [TaggedEntity] -> (Int, Int, Int)
gatherCountsForBIO expected got = (maxMatchOnOrdered laterThan expected got, length expected, length got)
  where
    laterThan (TaggedEntity (TaggedSpan a _) _ _) (TaggedEntity (TaggedSpan b _) _ _) = a > b

parseBioSequenceIntoEntities :: T.Text -> Either String [TaggedEntity]
parseBioSequenceIntoEntities t = labelsIntoEntities =<< (parseOnly (bioSequenceParser <* endOfInput) t)

labelsIntoEntities :: [BIOLabel] -> Either String [TaggedEntity]
labelsIntoEntities labels = labelsIntoEntities' $ zip labels [1..]

labelsIntoEntities' :: [(BIOLabel, Int)] -> Either String [TaggedEntity]
labelsIntoEntities' labelsWithPositions = mapM labelSplitToEntity labelsGathered
  where labelsGathered = splitLabels labelsWithPositions

labelSplitToEntity :: [(BIOLabel, Int)] -> Either String TaggedEntity
labelSplitToEntity labs@(h@(_,begIx):t) = if isBeginning h && all (\tp -> isInside tp && tt tp == btp) t
                                     then
                                       Right $ TaggedEntity (TaggedSpan begIx lastItemIx) btp mNormalized
                                     else
                                       Left "something wrong with label sequence"
  where isBeginning (Beginning _ _, _) = True
        isBeginning _ = False
        isInside (Inside _ _, _) = True
        isInside _ = False
        tt (Beginning t _, _) = t
        tt (Inside t _, _) = t
        btp = tt h
        lastItemIx = case t of
          [] -> begIx
          _ -> let (_, ix) = last t
              in ix
        normalized (Beginning _ n, _) = n
        normalized (Inside _ n, _) = n
        mNormalized = if all (\tp -> normalized tp == Nothing) labs
                        then
                          Nothing
                        else
                          Just $ T.intercalate "_" $ catMaybes $ map normalized labs

splitLabels :: [(BIOLabel, Int)] -> [[(BIOLabel, Int)]]
splitLabels [] = []
splitLabels ((Outside, _):r) = splitLabels r
splitLabels (e@(_, ix):r) =
  case splitLabels r of
    l@(((Beginning _ _, _):_):_) -> ([e]:l)
    (s@((Inside _ _, ix'):_):l) -> if ix' == ix + 1
                                    then
                                      ((e:s):l)
                                    else
                                      ([e]:(s:l))
    [] -> [[e]]

bioSequenceParser :: Parser [BIOLabel]
bioSequenceParser = sepByWhitespaces bioLabelParser

bioLabelParser :: Parser BIOLabel
bioLabelParser =
  (string "O" *> pure Outside) <|>
  (do
      labelType <- bioMarkerParser
      string "-"
      label <- takeWhile1 (\c -> not (isSpace c) && c /= '/')
      normalized <- (do
                       string "/"
                       normalized <- takeWhile1 (not . isSpace)
                       return $ Just normalized) <|> pure Nothing
      return $ labelType label normalized)

bioMarkerParser :: Parser (T.Text -> Maybe T.Text -> BIOLabel)
bioMarkerParser =
  (string "B" *> pure Beginning) <|> (string "I" *> pure Inside)
