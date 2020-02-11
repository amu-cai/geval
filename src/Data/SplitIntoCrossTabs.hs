{-# LANGUAGE OverloadedStrings #-}

module Data.SplitIntoCrossTabs
       (splitIntoCrossTabs,
        splitIntoTablesWithValues,
        CrossTab(..),
        TableWithValues(..),
        TextFrag(..))
       where


import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Set.Ordered as OS
import qualified Data.Map.Ordered as OM
import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Map.Lazy as LM
import Data.Maybe (fromMaybe)

import Data.List (unfoldr, sortBy, maximumBy, minimumBy)

data TableWithValues a = TableWithValues [Text] [(Text, [a])]
  deriving (Show)

data CrossTab = SingleItem Text | CrossTab [TextFrag] [TextFrag]
  deriving (Show, Eq)

data TextFrag = Prefix Text | Suffix Text
  deriving (Show, Eq, Ord)


informativeLookup m e = case m LM.!? e of
  Just x -> x
  Nothing -> error ("Cannot find " ++ (show e) ++ " in " ++ (show m))

splitIntoTablesWithValues :: Show a => Text
                            -> Text
                            -> LM.Map Text a -- ^ map from which values will be taken,
                                            -- deliberately a lazy map so that
                                            -- values could be shown one by one
                            -> [Text]
                            -> [TableWithValues a]
splitIntoTablesWithValues defaultMainHeader defaultSecondaryHeader mapping =
  joinSingleItems . map (convertIntoTableWithValues defaultMainHeader defaultSecondaryHeader mapping) . splitIntoCrossTabs
  where joinSingleItems (TableWithValues h@[_, _] arows : TableWithValues [_, _] brows : rest) =
          joinSingleItems (TableWithValues h (arows ++ brows) : rest)
        joinSingleItems (e : rest) = e : joinSingleItems rest
        joinSingleItems [] = []

convertIntoTableWithValues :: Show a => Text -> Text -> LM.Map Text a -> CrossTab -> TableWithValues a
convertIntoTableWithValues defaultMainHeader defaultSecondaryHeader mapping (SingleItem t) =
  TableWithValues [defaultMainHeader, defaultSecondaryHeader] [(t, [mapping LM.! t])]
convertIntoTableWithValues defaultMainHeader defaultSecondaryHeader mapping (CrossTab rowNames columnNames) =
  TableWithValues (T.empty : (map toText columnNames)) (map processRow rowNames)
  where processRow rowName = (toText rowName, map (\colName -> mapping `informativeLookup` (combineFrags rowName colName)) columnNames)

splitIntoCrossTabs :: [Text] -> [CrossTab]
splitIntoCrossTabs inputs =
  map preferVertical
  $ map snd
  $ sortBy (\(r1,_) (r2, _) -> r1 `compare` r2)
  $ map (getRank inputRanks)
  $ unfoldr extractTheBestCrossTab inputs
  where inputRanks = M.fromList $ zip inputs [1..]

preferVertical :: CrossTab -> CrossTab
preferVertical s@(SingleItem _) = s
preferVertical c@(CrossTab rowNames columnNames)
  | length rowNames < length columnNames = CrossTab columnNames rowNames
  | otherwise = c

getRank :: M.Map Text Int -> CrossTab -> (Int, CrossTab)
getRank ranks c = (bestRank, c)
  where bestRank = minimum
                   $ map (ranks M.!)
                   $ S.toList
                   $ toSet c

extractTheBestCrossTab :: [Text] -> Maybe (CrossTab, [Text])
extractTheBestCrossTab [] = Nothing
extractTheBestCrossTab ts = Just (theBestCrossTab, rest)
  where theBestCrossTab = findTheBestCrossTab ts
        rest = filter (`S.notMember` (toSet theBestCrossTab)) ts

findTheBestCrossTab :: [Text] -> CrossTab
findTheBestCrossTab ts = case orderedEntries of
  [] -> SingleItem defaultSingleton
  _ -> maximumBy (\t1 t2 -> crossTabSize t1 `compare` crossTabSize t2)
      $ map (findTheBestCrossTabForTextPart (SingleItem defaultSingleton) orderedEntries)
      $ map snd orderedEntries
  where mapping = gatherTextParts ts
        orderedEntries = sortBy entryComparator
                         $ filter (\(_, (_, tset)) -> OS.size tset >= 2)
                         $ zip [1..] (OM.assocs mapping)
        (defaultSingleton:_) = ts

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ o2 = o2
thenCmp o1 _  = o1

entryComparator (r1, (_, s1)) (r2, (_, s2)) = (OS.size s2 `compare` OS.size s1)
                                                      `thenCmp`
                                                    (r1 `compare` r2)

-- OS.|/\ is broken
(|/\) osetA osetB = OS.filter ((flip OS.member) osetB) osetA

findTheBestCrossTabForTextPart :: CrossTab -> [(Int, (TextFrag, OS.OSet TextFrag))] -> (TextFrag, OS.OSet TextFrag) -> CrossTab
findTheBestCrossTabForTextPart defaultCrossTab entries chosenEntry@(t, tset) = if crossTabSize bestCrossTabFound > 1
                                                                               then bestCrossTabFound
                                                                               else defaultCrossTab
  where bestCrossTabFound = foldr step (CrossTab [] (F.toList tset)) entriesOrderedByIntersection
        entriesOrderedByIntersection =
          sortBy entryComparator
          $ filter (\(_, (_, tset')) -> OS.size tset' >= 2)
          $ map (\(r, (t', tset')) -> (r, (t', tset' |/\ tset))) entries
        step (_, (t', tset')) currentTab@(CrossTab frags common) = selectedTab
            where newTab = CrossTab newT (F.toList newCommon)
                  newT = t':frags
                  newCommon = (OS.fromList common) |/\ tset'
                  selectedTab = if crossTabSize newTab >= crossTabSize currentTab
                                then newTab
                                else currentTab

crossTabSize :: CrossTab -> Int
crossTabSize (SingleItem _) = 1
crossTabSize (CrossTab [] _) = 0
crossTabSize (CrossTab _ []) = 0
-- tables really start from 2x2
crossTabSize (CrossTab [_] _) = 0
crossTabSize (CrossTab _ [_]) = 0
crossTabSize (CrossTab rows columns) = length rows * length columns

toSet :: CrossTab -> S.Set Text
toSet (SingleItem t) = S.singleton t
toSet (CrossTab rowNames columnNames) = S.fromList [rName `combineFrags` cName | rName <- rowNames, cName <- columnNames]

toText :: TextFrag -> Text
toText (Prefix prefix) = T.stripEnd prefix
toText (Suffix prefix) = T.stripStart prefix

combineFrags :: TextFrag -> TextFrag -> Text
combineFrags (Prefix prefix) (Suffix suffix) = prefix <> suffix
combineFrags (Suffix suffix) (Prefix prefix) = prefix <> suffix
combineFrags _ _ = error $ "incompatible text fragments"

getTextParts :: Text -> [(TextFrag, TextFrag)]
getTextParts t = [(Prefix (T.take ix t), Suffix (T.drop ix t)) | ix <- [1..(T.length t)-1]]

gatherTextParts :: [Text] -> OM.OMap TextFrag (OS.OSet TextFrag)
gatherTextParts = (gather OS.singleton (OS.|<>)) . concat . (map getTextParts)

gather :: Ord a => (b -> c) -> (c -> c -> c) -> [(a, b)] -> OM.OMap a c
gather createEntry combine = foldr extend OM.empty
  where extend (k, v) m = OM.unionWithL (\_ v1 v2 -> combine v1 v2) (OM.singleton (k, (createEntry v))) m
