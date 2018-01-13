{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module GEval.LineByLine
       (runLineByLine
       ) where

import GEval.Core

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import Data.Text
import Data.Text.Encoding

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Data.Word

import Text.Printf

data LineRecord = LineRecord Text Text Text Word32 MetricValue
                  deriving (Eq, Show)

runLineByLine :: GEvalSpecification -> IO ()
runLineByLine spec = do
  (inputFilePath, expectedFilePath, outFilePath) <- checkAndGetFiles spec
  gevalLineByLineCore metric inputFilePath expectedFilePath outFilePath consum
   where metric = gesMetric spec
         justScore (LineRecord _ _ _ _ score) = score
         consum :: Consumer LineRecord (ResourceT IO) ()
         consum = (CL.map (encodeUtf8 . formatOutput) =$= CC.unlinesAscii =$= CC.stdout)
         formatOutput (LineRecord inp exp out _ score) = Data.Text.intercalate "\t" [
           formatScore score,
           escapeTabs inp,
           escapeTabs exp,
           escapeTabs out]
         formatScore :: MetricValue -> Text
         formatScore = Data.Text.pack . printf "%f"
         escapeTabs = Data.Text.replace "\t" "<tab>"

gevalLineByLineCore :: Metric -> FilePath -> FilePath -> FilePath -> Sink LineRecord (ResourceT IO) () -> IO ()
gevalLineByLineCore metric inputFilePath expectedFilePath outFilePath consum =
  runResourceT $
     ((getZipSource $ (,)
       <$> ZipSource (CL.sourceList [1..])
       <*> (ZipSource $ recordSource context parserSpec)) =$= CL.mapM (checkStepM evaluateLine) =$= CL.catMaybes $$ consum)
  where parserSpec = (ParserSpecWithInput (Right . id) (Right . id) (Right . id))
        context = (WithInput inputLineSource expectedLineSource outputLineSource)
        inputLineSource = fileAsLineSource inputFilePath
        expectedLineSource = fileAsLineSource expectedFilePath
        outputLineSource = fileAsLineSource outFilePath
        justLine (LineInFile _ _ l) = l
        evaluateLine (lineNo, ParsedRecordWithInput inp exp out) = do
          s <- liftIO $ gevalCoreOnSingleLines metric (LineInFile inputFilePath lineNo inp)
                                                     (LineInFile expectedFilePath lineNo exp)
                                                     (LineInFile outFilePath lineNo out)
          return $ LineRecord inp exp out lineNo s
