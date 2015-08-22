module Main where

import GEval
import System.Environment

import Options.Applicative
import OptionsParser

main :: IO ()
main = do
  opts <- execParser fullOptionsParser
  result <- geval $ geoSpec opts
  print $ result
