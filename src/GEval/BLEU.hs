module GEval.BLEU
       (precisionCount, bleuStep, gleuStep)
       where

import GEval.Common
import GEval.PrecisionRecall

import qualified Data.MultiSet as MS
import Data.List (minimumBy, maximumBy, zip, zip3, zip4)

bleuStep :: Ord a => [[a]] -> [a] -> (Int, Int, Int, Int,  Int,  Int, Int, Int, Int)
bleuStep refs trans = (prec1, prec2, prec3, prec4,  closestLen, len1, len2, len3, len4)
  where prec1 = precisionCountForNgrams id
        prec2 = precisionCountForNgrams bigrams
        prec3 = precisionCountForNgrams trigrams
        prec4 = precisionCountForNgrams tetragrams
        precisionCountForNgrams fun = precisionCount (map fun refs) (fun trans)
        closestLen = minimumBy closestCmp $ map length refs
        closestCmp x y
          | ((abs (x - len1)) < (abs (y - len1))) = LT
          | ((abs (x - len1)) > (abs (y - len1))) = GT
          | ((abs (x - len1)) == (abs (y - len1))) = x `compare` y
        len1 = length trans
        len2 = max 0 (len1 - 1)
        len3 = max 0 (len1 - 2)
        len4 = max 0 (len1 - 3)

gleuStep :: Ord a => [[a]] -> [a] -> (Int, Int)
gleuStep refs trans = maximumBy (\(g1, t1) (g2, t2) -> (g1 /. t1) `compare` (g2 /. t2)) $ map getBetterCounts refs
  where getBetterCounts ref = let (matched1, expected1, got1) = getSimpleCounts ref trans1grams
                                  (matched2, expected2, got2) = getSimpleCounts (bigrams ref) trans2grams
                                  (matched3, expected3, got3) = getSimpleCounts (trigrams ref) trans3grams
                                  (matched4, expected4, got4) = getSimpleCounts (tetragrams ref) trans4grams
                                  total = max (expected1 + expected2 + expected3 + expected4) (got1 + got2 + got3 + got4)
                              in (matched1 + matched2 + matched3 + matched4, total)
        (trans1grams, trans2grams, trans3grams, trans4grams) = upToTetragrams trans

getSimpleCounts :: Ord a => [a] -> [a] -> (Int, Int, Int)
getSimpleCounts expected got = (countMatched expected got,
                                length expected,
                                length got)

countMatched :: Ord a => [a] -> [a] -> Int
countMatched ref got = sum $ map (lookFor rf) $ MS.toOccurList $ MS.fromList got
  where rf = MS.fromList ref
        lookFor rf (e, freq) = min freq (MS.occur e rf)

precisionCount :: Ord a => [[a]] -> [a] -> Int
precisionCount refs = sum . map (lookFor refs) . MS.toOccurList . MS.fromList
  where lookFor refs (e, freq) = minimumOrZero $ filter (> 0) $ map (findE e freq) $ map MS.fromList refs
        findE e freq m = min freq (MS.occur e m)
        minimumOrZero [] = 0
        minimumOrZero l = minimum l

upToTetragrams :: [a] -> ([a], [(a, a)], [(a, a, a)], [(a, a, a, a)])
upToTetragrams l = (l, bigrams l, trigrams l, tetragrams l)

trigrams :: [a] -> [(a, a, a)]
trigrams [] = []
trigrams [_] = []
trigrams [_, _] = []
trigrams u = zip3 u (tail u) (tail $ tail u)

tetragrams :: [a] -> [(a, a, a, a)]
tetragrams [] = []
tetragrams [_] = []
tetragrams [_, _] = []
tetragrams [_, _, _] = []
tetragrams u = zip4 u (tail u) (tail $ tail u) (tail $ tail $ tail u)
