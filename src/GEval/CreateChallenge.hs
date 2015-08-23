{-# LANGUAGE QuasiQuotes #-}

module GEval.CreateChallenge
       (createChallenge)
       where

import GEval.Core
import qualified System.Directory as D
import Control.Conditional (whenM)

import System.IO
import Control.Exception
import Control.Monad.Trans.Resource

createChallenge :: FilePath -> GEvalSpecification -> IO ()
createChallenge expectedDirectory spec = putStrLn "creating challange..."

createFile :: FilePath -> String -> IO ()
createFile filePath contents = do
    whenM (D.doesFileExist filePath) $ throwM $ FileAlreadyThere filePath
    writeFile filePath contents
