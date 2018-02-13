{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module GEval.LineByLine
       (runLineByLine,
        runDiff
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
         consum :: Consumer LineRecord (ResourceT IO) ()
         consum = (CL.map (encodeUtf8 . formatOutput) =$= CC.unlinesAscii =$= CC.stdout)
         formatOutput (LineRecord inp exp out _ score) = Data.Text.intercalate "\t" [
           formatScore score,
           escapeTabs inp,
           escapeTabs exp,
           escapeTabs out]
         formatScore :: MetricValue -> Text
         formatScore = Data.Text.pack . printf "%f"

runDiff :: FilePath -> GEvalSpecification -> IO ()
runDiff otherOut spec = do
  let otherOutFilePath = getOutFile spec otherOut
  (inputFilePath, expectedFilePath, outFilePath) <- checkAndGetFiles spec
  let sourceA = gevalLineByLineSource metric inputFilePath expectedFilePath outFilePath
  let sourceB = gevalLineByLineSource metric inputFilePath expectedFilePath otherOutFilePath
  runResourceT $
     ((getZipSource $ (,)
       <$> ZipSource sourceA
       <*> ZipSource sourceB) $$ consum)
  where metric = gesMetric spec
        consum :: Consumer (LineRecord, LineRecord) (ResourceT IO) ()
        consum = (CL.filter shouldBeShown =$= CL.map (encodeUtf8 . formatOutput) =$= CC.unlinesAscii =$= CC.stdout)
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

escapeTabs :: Text -> Text
escapeTabs = Data.Text.replace "\t" "<tab>"

gevalLineByLineCore :: Metric -> FilePath -> FilePath -> FilePath -> Sink LineRecord (ResourceT IO) () -> IO ()
gevalLineByLineCore metric inputFilePath expectedFilePath outFilePath consum =
  runResourceT $
     ((gevalLineByLineSource metric inputFilePath expectedFilePath outFilePath) $$ consum)

gevalLineByLineSource :: Metric -> FilePath -> FilePath -> FilePath -> Source (ResourceT IO) LineRecord
gevalLineByLineSource metric inputFilePath expectedFilePath outFilePath =
  (getZipSource $ (,)
       <$> ZipSource (CL.sourceList [1..])
       <*> (ZipSource $ recordSource context parserSpec)) =$= CL.mapM (checkStepM evaluateLine) =$= CL.catMaybes
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
