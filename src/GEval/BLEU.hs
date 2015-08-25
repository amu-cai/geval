module GEval.BLEU
       (precisionCount, bleuStep)
       where

import qualified Data.MultiSet as MS
import Data.List (minimumBy, zip, zip3, zip4)

import Debug.Trace



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
        bigrams [] = []
        bigrams [_] = []
        bigrams u = zip u $ tail u
        trigrams [] = []
        trigrams [_] = []
        trigrams [_, _] = []
        trigrams u = zip3 u (tail u) (tail $ tail u)
        tetragrams [] = []
        tetragrams [_] = []
        tetragrams [_, _] = []
        tetragrams [_, _, _] = []
        tetragrams u = zip4 u (tail u) (tail $ tail u) (tail $ tail $ tail u)

precisionCount :: Ord a => [[a]] -> [a] -> Int
precisionCount refs = sum . map (lookFor refs) . MS.toOccurList . MS.fromList
  where lookFor refs (e, freq) = minimumOrZero $ filter (> 0) $ map (findE e freq) $ map MS.fromList refs
        findE e freq m = min freq (MS.occur e m)
        minimumOrZero [] = 0
        minimumOrZero l = minimum l
