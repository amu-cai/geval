{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module GEval.LineByLine
       (runLineByLine,
        runLineByLineGeneralized,
        runDiff,
        runDiffGeneralized
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
runLineByLine spec = runLineByLineGeneralized spec consum
   where consum :: ConduitT LineRecord Void (ResourceT IO) ()
         consum = (CL.map (encodeUtf8 . formatOutput) .| CC.unlinesAscii .| CC.stdout)
         formatOutput (LineRecord inp exp out _ score) = Data.Text.intercalate "\t" [
           formatScore score,
           escapeTabs inp,
           escapeTabs exp,
           escapeTabs out]
         formatScore :: MetricValue -> Text
         formatScore = Data.Text.pack . printf "%f"

runLineByLineGeneralized :: GEvalSpecification -> ConduitT LineRecord Void (ResourceT IO) a -> IO a
runLineByLineGeneralized spec consum = do
  (inputFilePath, expectedFilePath, outFilePath) <- checkAndGetFiles spec
  gevalLineByLineCore metric inputFilePath expectedFilePath outFilePath consum
  where metric = gesMetric spec

runDiff :: FilePath -> GEvalSpecification -> IO ()
runDiff otherOut spec = runDiffGeneralized otherOut spec consum
  where consum :: ConduitT (LineRecord, LineRecord) Void (ResourceT IO) ()
        consum = (CL.filter shouldBeShown .| CL.map (encodeUtf8 . formatOutput) .| CC.unlinesAscii .| CC.stdout)
        shouldBeShown (LineRecord _ _ outA _ scoreA, LineRecord _ _ outB _ scoreB) =
          outA /= outB && scoreA /= scoreB
        formatOutput (LineRecord inp exp outA _ scoreA, LineRecord _ _ outB _ scoreB) = Data.Text.intercalate "\t" [
          formatScoreDiff (scoreB - scoreA),
          escapeTabs inp,
          escapeTabs exp,
          escapeTabs outA,
          escapeTabs outB]
        formatScoreDiff :: Double -> Text
        formatScoreDiff = Data.Text.pack . printf "%f"

runDiffGeneralized :: FilePath -> GEvalSpecification -> ConduitT (LineRecord, LineRecord) Void (ResourceT IO) a -> IO a
runDiffGeneralized otherOut spec consum = do
  let otherOutFilePath = getOutFile spec otherOut
  (inputFilePath, expectedFilePath, outFilePath) <- checkAndGetFiles spec
  let sourceA = gevalLineByLineSource metric inputFilePath expectedFilePath outFilePath
  let sourceB = gevalLineByLineSource metric inputFilePath expectedFilePath otherOutFilePath
  runResourceT $ runConduit $
     ((getZipSource $ (,)
       <$> ZipSource sourceA
       <*> ZipSource sourceB) .| consum)
  where metric = gesMetric spec

escapeTabs :: Text -> Text
escapeTabs = Data.Text.replace "\t" "<tab>"

gevalLineByLineCore :: Metric -> FilePath -> FilePath -> FilePath -> ConduitT LineRecord Void (ResourceT IO) a -> IO a
gevalLineByLineCore metric inputFilePath expectedFilePath outFilePath consum =
  runResourceT $ runConduit $
     ((gevalLineByLineSource metric inputFilePath expectedFilePath outFilePath) .| consum)

gevalLineByLineSource :: Metric -> FilePath -> FilePath -> FilePath -> ConduitT () LineRecord (ResourceT IO) ()
gevalLineByLineSource metric inputFilePath expectedFilePath outFilePath =
  (getZipSource $ (,)
       <$> ZipSource (CL.sourceList [1..])
       <*> (ZipSource $ recordSource context parserSpec)) .| CL.mapM (checkStepM evaluateLine) .| CL.catMaybes
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
