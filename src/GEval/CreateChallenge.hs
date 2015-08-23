{-# LANGUAGE QuasiQuotes #-}

module GEval.CreateChallenge
       (createChallenge)
       where

import GEval.Core

createChallenge :: FilePath -> GEvalSpecification -> IO ()
createChallenge expectedDirectory spec = putStrLn "creating challange..."
