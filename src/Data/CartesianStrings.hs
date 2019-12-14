{-# LANGUAGE OverloadedStrings #-}

module Data.CartesianStrings
       (parseCartesianString,
        concatCartesianStrings,
        CartesianStrings(..))
       where

import Data.List (findIndex)
import Data.List.Split (splitOn)

-- A helper library for parsing strings representing sets of strings
-- obtained via a Cartesian product, e.g.:
-- - "foo" represents just ["foo"]
-- - "a-{foo,bar,baz}-b" represents ["a-foo-b", "a-bar-b", "a-baz-b"]
-- - "{foo,bar,baz}-{x,y}-{0,1,2}" represents a set containing 18 strings

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

parseCartesianString :: String -> [String]
parseCartesianString s =
  case findIndex (=='{') s of
    Just begIx ->
      let pref = take begIx s
          c = drop (begIx + 1) s
      in case findIndex (=='}') c of
           Just endIx ->
             let inf = take endIx c
                 current = splitOn "," inf
                 rest = parseCartesianString $ drop (endIx + 1) c
             in map (uncurry (++)) $ cartProd (map (pref++) current) rest
    Nothing -> [s]

data CartesianStrings a = CartesianStrings [a]
  deriving (Eq)

instance Read a => Read (CartesianStrings a) where
  readsPrec _ s = [(CartesianStrings (map read $ parseCartesianString s), "")]

concatCartesianStrings :: [CartesianStrings a] -> [a]
concatCartesianStrings = concat . map (\(CartesianStrings ss) -> ss)
