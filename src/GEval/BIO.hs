{-# LANGUAGE OverloadedStrings #-}

module GEval.BIO
       (BIOLabel(..), bioSequenceParser, parseBioSequenceIntoEntities,
        parseBioSequenceIntoEntitiesWithoutNormalization,
        TaggedSpan(..), TaggedEntity(..), gatherCountsForBIO, gatherSeparatedCountsForBIO,
        eraseNormalisation)
       where

import GEval.PrecisionRecall

import qualified Data.Text as T

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Char
import Data.Maybe (catMaybes)
import Data.List (groupBy, sortBy)

import GEval.Common

import qualified Data.HashMap.Strict as M

data BIOLabel = Outside | Beginning T.Text (Maybe T.Text) | Inside T.Text (Maybe T.Text)
                deriving (Eq, Show)

formatBioLabel :: BIOLabel -> T.Text
formatBioLabel Outside = "O"
formatBioLabel (Beginning label Nothing) = T.concat ["B-", label]
formatBioLabel (Beginning label (Just normalized)) = T.concat ["B-", label, "/", normalized]
formatBioLabel (Inside label Nothing) = T.concat ["I-", label]
formatBioLabel (Inside label (Just normalized)) = T.concat ["I-", label, "/", normalized]

data TaggedSpan = TaggedSpan Int Int
                  deriving (Eq, Show)

data TaggedEntity = TaggedEntity TaggedSpan T.Text (Maybe T.Text)
                    deriving (Eq, Show)

eraseNormalisation :: TaggedEntity -> TaggedEntity
eraseNormalisation (TaggedEntity span label normalized) = (TaggedEntity span label Nothing)

gatherCountsForBIO :: [TaggedEntity] -> [TaggedEntity] -> (Int, Int, Int)
gatherCountsForBIO expected got = (maxMatchOnOrdered laterThan expected got, length expected, length got)
  where
    laterThan (TaggedEntity (TaggedSpan a _) _ _) (TaggedEntity (TaggedSpan b _) _ _) = a > b

compareByLabel :: TaggedEntity -> TaggedEntity -> Ordering
compareByLabel (TaggedEntity _ labelA _) (TaggedEntity _ labelB _) = labelA `compare` labelB

equalLabel :: TaggedEntity -> TaggedEntity -> Bool
equalLabel (TaggedEntity _ labelA _) (TaggedEntity _ labelB _) = labelA == labelB

gatherSeparatedCountsForBIO :: [TaggedEntity] -> [TaggedEntity] -> M.HashMap T.Text (Int, Int, Int)
gatherSeparatedCountsForBIO expected got = M.mapWithKey process expectedMapped
  where expectedMapped = groupEntitiesByLabel expected
        gotMapped = groupEntitiesByLabel got
        groupEntitiesByLabel =
          M.fromList
          . map (\l@((TaggedEntity _ lab _):_) -> (lab, l))
          . groupBy equalLabel
          . sortBy compareByLabel
        process lab expectedGroup = gatherCountsForBIO expectedGroup (M.lookupDefault [] lab gotMapped)

parseBioSequenceIntoEntities :: T.Text -> Either String [TaggedEntity]
parseBioSequenceIntoEntities t = labelsIntoEntities =<< (parseOnly (bioSequenceParser <* endOfInput) t)

parseBioSequenceIntoEntitiesWithoutNormalization s = do
  entities <- parseBioSequenceIntoEntities s
  return $ Prelude.map eraseNormalisation entities

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
                                       Left $ "inconsistent label sequence `" ++ (T.unpack $ T.intercalate " " $ map (formatBioLabel . fst) labs)  ++ "`"
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
      (string "-" <|> string "_")
      label <- takeWhile1 (\c -> not (isSpace c) && c /= '/')
      normalized <- (do
                       string "/"
                       normalized <- takeWhile1 (not . isSpace)
                       return $ Just normalized) <|> pure Nothing
      return $ labelType label normalized)

bioMarkerParser :: Parser (T.Text -> Maybe T.Text -> BIOLabel)
bioMarkerParser =
  (string "B" *> pure Beginning) <|> (string "I" *> pure Inside)
