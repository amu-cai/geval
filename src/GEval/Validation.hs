{-# LANGUAGE OverloadedStrings #-}

module GEval.Validation
    ( validationChallenge
    ) where

import GEval.Metric
import GEval.EvaluationScheme
import GEval.Core (GEvalSpecification(..), isEmptyFile, geval, defaultInputFile, defaultExpectedFile, defaultOutFile)
import GEval.Common
import qualified System.Directory as D

import System.FilePath.Find as SFF
import System.FilePath
import System.Directory
import Control.Exception
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Conditional (unlessM, whenM, unless, when)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Text as CT
import Data.Conduit.Binary (sourceFile, sinkFile)
import Data.Conduit.AutoDecompress (autoDecompress)
import Data.Conduit.SmartSource (compressedFilesHandled)
import Data.List (find, intercalate, nub)
import qualified Data.Text as T
import Data.Maybe (isJust)

import System.IO.Temp

data ValidationException = NoChallengeDirectory FilePath
                         | NoFoundFile FilePath
                         | NoConfigFile FilePath
                         | NoReadmeFile FilePath
                         | NoGitignoreFile FilePath
                         | EmptyFile FilePath
                         | VaryingNumberOfLines
                         | NoTestDirectories
                         | TooManyInputFiles [FilePath]
                         | TooManyExpectedFiles [FilePath]
                         | TooManyTrainFiles [FilePath]
                         | OutputFileDetected [FilePath]
                         | CharacterCRDetected FilePath
                         | SpaceSuffixDetect FilePath
                         | VaryingNumberOfColumns FilePath
                         | BestPossibleValueNotObtainedWithExpectedData MetricValue MetricValue
                         | NoMetricOfPriorityOne

instance Exception ValidationException

instance Show ValidationException where
  show (NoChallengeDirectory filePath) = somethingWrongWithFilesMessage "No challenge directory" filePath
  show (NoFoundFile filePath) = somethingWrongWithFilesMessage "No file found" filePath
  show (NoConfigFile filePath) = somethingWrongWithFilesMessage "No config.txt file" filePath
  show (NoReadmeFile filePath) = somethingWrongWithFilesMessage "No README.md file" filePath
  show (NoGitignoreFile filePath) = somethingWrongWithFilesMessage "No .gitignore file" filePath
  show (EmptyFile filePath) = somethingWrongWithFilesMessage "Empty file" filePath
  show VaryingNumberOfLines = "The number of lines in input and expected file is not the same"
  show NoTestDirectories = "No directories with test data, expected `dev-0` and/or `test-A` directory"
  show (TooManyInputFiles filePaths) = somethingWrongWithFilesMessage "Too many input files" $ intercalate "`, `" filePaths
  show (TooManyExpectedFiles filePaths) = somethingWrongWithFilesMessage "Too many expected files" $ intercalate "`, `" filePaths
  show (TooManyTrainFiles filePaths) = somethingWrongWithFilesMessage "Too many train files" $ intercalate "`, `" filePaths
  show (OutputFileDetected filePaths) = somethingWrongWithFilesMessage "Output file/s detected" $ intercalate "`, `" filePaths
  show (CharacterCRDetected filePaths) = somethingWrongWithFilesMessage "Found CR (Carriage Return, 0x0D) character" filePaths
  show (SpaceSuffixDetect filePaths) = somethingWrongWithFilesMessage "Found space at the end of line" filePaths
  show (VaryingNumberOfColumns filePaths) = somethingWrongWithFilesMessage "The file contains varying number of columns" filePaths
  show (BestPossibleValueNotObtainedWithExpectedData expected got) = "The best possible value was not obtained with the expected data, expected: " ++ (show expected) ++ " , obtained: " ++ (show got)
  show NoMetricOfPriorityOne = "No metric has priority 1"

hasMetricOfPriorityOne :: GEvalSpecification -> Bool
hasMetricOfPriorityOne spec = isJust $ Data.List.find (\m -> (evaluationSchemePriority m) == 1) $ gesMetrics spec

validationChallenge :: FilePath -> GEvalSpecification -> IO ()
validationChallenge challengeDirectory spec = do
  unlessM (D.doesDirectoryExist challengeDirectory) $ throwM $ NoChallengeDirectory challengeDirectory
  unlessM (D.doesFileExist configFile) $ throwM $ NoConfigFile configFile
  unlessM (D.doesFileExist gitignoreFile) $ throwM $ NoGitignoreFile gitignoreFile
  unlessM (D.doesFileExist readmeFile) $ throwM $ NoReadmeFile readmeFile
  unless (hasMetricOfPriorityOne spec) $ throwM NoMetricOfPriorityOne
  checkCorrectFile configFile
  checkCorrectFile gitignoreFile
  checkCorrectFile readmeFile
  testDirectories <- findTestDirs challengeDirectory
  checkTestDirectories spec testDirectories
  checkTrainDirectory spec challengeDirectory

  where
    configFile = challengeDirectory </> "config.txt"
    gitignoreFile = challengeDirectory </> ".gitignore"
    readmeFile = challengeDirectory </> "README.md"

checkCorrectFile :: FilePath -> IO ()
checkCorrectFile filePath = do
  whenM (isEmptyFile filePath) $ throwM $ EmptyFile filePath
  lines' <- getFileLines filePath
  let lines = map T.pack lines'
  when (any (T.isInfixOf "\r") lines) $ throw $ CharacterCRDetected filePath
  when (any (T.isSuffixOf " ") lines) $ throw $ SpaceSuffixDetect filePath

getFileLines :: FilePath -> IO [String]
getFileLines file = runResourceT $ runConduit (sourceFile file
                                               .| autoDecompress
                                               .| CC.decodeUtf8
                                               .| CT.lines
                                               .| CC.map T.unpack
                                               .| CL.consume)

countLines :: FilePath -> IO Int
countLines file = do
  lines <- getFileLines file
  return $ length lines

numberOfColumns :: FilePath -> IO [Int]
numberOfColumns file = runResourceT $ runConduit (sourceFile file
                                                  .| autoDecompress
                                                  .| CC.decodeUtf8
                                                  .| CT.lines
                                                  .| CC.map (\t -> length $ T.splitOn "\t" t)
                                                  .| CL.consume)

createPerfectOutputFromExpected :: Metric -> FilePath -> FilePath -> IO ()
createPerfectOutputFromExpected metric expectedFile outFile = do
  runResourceT $ runConduit $ (sourceFile expectedFile
                                .| autoDecompress
                                .| CC.decodeUtf8
                                .| CT.lines
                                .| CC.map (perfectOutLineFromExpectedLine metric)
                                .| CC.unlines
                                .| CC.encodeUtf8
                                .| sinkFile outFile)


findTestDirs :: FilePath -> IO [FilePath]
findTestDirs = SFF.find never testDirFilter

findInputFiles :: FilePath -> IO [FilePath]
findInputFiles = SFF.find never $ fileFilter defaultInputFile

findOutputFiles :: FilePath -> IO [FilePath]
findOutputFiles = SFF.find never $ fileFilter "out*.tsv"

findTrainFiles :: FilePath -> IO [FilePath]
findTrainFiles = SFF.find never $ fileFilter "train.tsv"

findExpectedFiles :: FilePath -> IO [FilePath]
findExpectedFiles = SFF.find never $ fileFilter defaultExpectedFile

never :: FindClause Bool
never = depth ==? 0

testDirFilter :: FindClause Bool
testDirFilter = (SFF.fileType ==? Directory) &&? (SFF.fileName ~~? "dev-*"
                                                  ||? SFF.fileName ~~? "test-*")

fileFilter :: String -> FindClause Bool
fileFilter fileName = (SFF.fileType ==? RegularFile) &&? (SFF.fileName ~~? fileName ||? SFF.fileName ~~? fileName ++ exts)
  where
    exts = Prelude.concat [ "(", intercalate "|" compressedFilesHandled, ")" ]


checkTestDirectories :: GEvalSpecification -> [FilePath] -> IO ()
checkTestDirectories _ [] = throwM NoTestDirectories
checkTestDirectories spec directories = mapM_ (checkTestDirectory spec) directories

checkTestDirectory :: GEvalSpecification -> FilePath -> IO ()
checkTestDirectory spec directoryPath = do
  inputFiles <- findInputFiles directoryPath
  when (null inputFiles) $ throw $ NoInputFile inputFile
  when (length inputFiles > 1) $ throw $ TooManyInputFiles inputFiles
  checkCorrectFile $ head inputFiles
  when (fixedNumberOfColumnsInInput metric) $ checkColumns $ head inputFiles

  expectedFiles <- findExpectedFiles directoryPath
  when (null expectedFiles) $ throw $ NoExpectedFile expectedFile
  when (length expectedFiles > 1) $ throw $ TooManyExpectedFiles expectedFiles
  checkCorrectFile $ head expectedFiles
  when (fixedNumberOfColumnsInExpected metric) $ checkColumns $ head expectedFiles

  inputLines <- countLines $ head inputFiles
  expectedLines <- countLines $ head expectedFiles

  when (inputLines /= expectedLines) $ throw $ VaryingNumberOfLines

  outputFiles <- findOutputFiles directoryPath
  unless (null outputFiles) $ throw $ OutputFileDetected outputFiles

  runOnTest spec directoryPath

  where
    metric = evaluationSchemeMetric $ head $ gesMetrics spec
    inputFile = directoryPath </> defaultInputFile

    expectedFile = directoryPath </> defaultExpectedFile

checkTrainDirectory :: GEvalSpecification -> FilePath -> IO ()
checkTrainDirectory spec challengeDirectory = do
  let trainDirectory = challengeDirectory </> "train"
  whenM (doesDirectoryExist trainDirectory) $ do
    trainFiles <- findTrainFiles trainDirectory
    if (not $ null trainFiles)
    then
     do
      putStrLn "WARNING: Found old-style train file `train.tsv`, whereas the same convention as in"
      putStrLn "WARNING: test directories if preferred (`in.tsv` and `expected.tsv`)."
      putStrLn "WARNING: (Though, there might still be some cases when `train.tsv` is needed, e.g. for training LMs.)"
    else
     do
      checkTestDirectory spec trainDirectory

checkColumns :: FilePath -> IO ()
checkColumns filePath = do
  columns <- numberOfColumns filePath
  let uniqueColumns = nub columns
  when (length uniqueColumns > 1) $ throw $ VaryingNumberOfColumns filePath

runOnTest :: GEvalSpecification -> FilePath -> IO ()
runOnTest spec testPath = do
  [expectedFile] <- findExpectedFiles testPath
  let testName = takeFileName testPath
  let modifiedSpec = spec {
        gesExpectedDirectory = Just (takeDirectory testPath),
        gesTestName = testName
        }

  (flip mapM_) (gesMetrics spec) $ \scheme -> do
    withSystemTempDirectory "geval-validation" $ \tmpDir -> do
      let metric = evaluationSchemeMetric scheme
      let tmpOutDir = tmpDir </> testName
      let tmpOutFile = tmpOutDir </> defaultOutFile
      createDirectory tmpOutDir
      let specificSpec = modifiedSpec {
            gesMetrics = [scheme],
            gesOutDirectory = tmpDir }
      createPerfectOutputFromExpected metric expectedFile tmpOutFile
      [(_, [MetricOutput value _])] <- geval specificSpec
      let bestValue = bestPossibleValue metric
      unless (bestValue =~ (extractSimpleRunValue value)) $ throw $ BestPossibleValueNotObtainedWithExpectedData bestValue (extractSimpleRunValue value)
      return ()
