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
  where getBetterCounts ref = let (matched, expected, got) = getCounts (==) (upToTetragrams ref, transNgrams)
                                  total = max expected got
                              in (matched, total)
        transNgrams = upToTetragrams trans

precisionCount :: Ord a => [[a]] -> [a] -> Int
precisionCount refs = sum . map (lookFor refs) . MS.toOccurList . MS.fromList
  where lookFor refs (e, freq) = minimumOrZero $ filter (> 0) $ map (findE e freq) $ map MS.fromList refs
        findE e freq m = min freq (MS.occur e m)
        minimumOrZero [] = 0
        minimumOrZero l = minimum l

data Ngram a = Unigram a | Bigram (a, a) | Trigram (a, a, a) | Tetragram (a, a, a, a)
               deriving (Eq, Show)

upToTetragrams :: [a] -> [Ngram a]
upToTetragrams l = (map Unigram l)
                   ++ (map Bigram $ bigrams l)
                   ++ (map Trigram $ trigrams l)
                   ++ (map Tetragram $ tetragrams l)

bigrams :: [a] -> [(a, a)]
bigrams [] = []
bigrams [_] = []
bigrams u = zip u $ tail u

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
