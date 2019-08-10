{-# LANGUAGE OverloadedStrings #-}

module GEval.Validation
    ( validationChallenge
    ) where

import GEval.Metric
import GEval.Core (GEvalSpecification(..), GEvalException(..), somethingWrongWithFilesMessage, isEmptyFile, geval, defaultInputFile, defaultExpectedFile, defaultOutFile)
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
import Data.List (intercalate)
import qualified Data.Text as T

import System.IO.Temp

data ValidationException = NoChallengeDirectory FilePath
                         | NoFoundFile FilePath
                         | NoConfigFile FilePath
                         | NoReadmeFile FilePath
                         | NoGitignoreFile FilePath
                         | EmptyFile FilePath
                         | NoTestDirectories
                         | TooManyInputFiles [FilePath]
                         | TooManyExpectedFiles [FilePath]
                         | OutputFileDetected [FilePath]
                         | CharacterCRDetected FilePath
                         | SpaceSuffixDetect FilePath
                         | BestPossibleValueNotObtainedWithExpectedData MetricValue MetricValue

instance Exception ValidationException

instance Show ValidationException where
  show (NoChallengeDirectory filePath) = somethingWrongWithFilesMessage "No challenge directory" filePath
  show (NoFoundFile filePath) = somethingWrongWithFilesMessage "No file found" filePath
  show (NoConfigFile filePath) = somethingWrongWithFilesMessage "No config.txt file" filePath
  show (NoReadmeFile filePath) = somethingWrongWithFilesMessage "No README.md file" filePath
  show (NoGitignoreFile filePath) = somethingWrongWithFilesMessage "No .gitignore file" filePath
  show (EmptyFile filePath) = somethingWrongWithFilesMessage "Empty file" filePath
  show NoTestDirectories = "No directories with test data, expected `dev-0` and/or `test-A` directory"
  show (TooManyInputFiles filePaths) = somethingWrongWithFilesMessage "Too many input files" $ intercalate "`, `" filePaths
  show (TooManyExpectedFiles filePaths) = somethingWrongWithFilesMessage "Too many expected files" $ intercalate "`, `" filePaths
  show (OutputFileDetected filePaths) = somethingWrongWithFilesMessage "Output file/s detected" $ intercalate "`, `" filePaths
  show (CharacterCRDetected filePaths) = somethingWrongWithFilesMessage "Found CR (Carriage Return, 0x0D) character" filePaths
  show (SpaceSuffixDetect filePaths) = somethingWrongWithFilesMessage "Found space at the end of line" filePaths
  show (BestPossibleValueNotObtainedWithExpectedData expected got) = "The best possible value was not obtained with the expected data, expected: " ++ (show expected) ++ " , obtained: " ++ (show got)

validationChallenge :: FilePath -> GEvalSpecification -> IO ()
validationChallenge challengeDirectory spec = do
  unlessM (D.doesDirectoryExist challengeDirectory) $ throwM $ NoChallengeDirectory challengeDirectory
  unlessM (D.doesFileExist configFile) $ throwM $ NoConfigFile configFile
  unlessM (D.doesFileExist gitignoreFile) $ throwM $ NoGitignoreFile gitignoreFile
  unlessM (D.doesFileExist readmeFile) $ throwM $ NoReadmeFile readmeFile
  checkCorrectFile configFile
  checkCorrectFile gitignoreFile
  checkCorrectFile readmeFile
  testDirectories <- findTestDirs challengeDirectory
  checkTestDirectories testDirectories

  mapM_ (runOnTest spec) testDirectories

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

findExpectedFiles :: FilePath -> IO [FilePath]
findExpectedFiles = SFF.find never $ fileFilter defaultExpectedFile

never :: FindClause Bool
never = depth ==? 0

testDirFilter :: FindClause Bool
testDirFilter = (SFF.fileType ==? Directory) &&? (SFF.fileName ~~? "dev-*" ||? SFF.fileName ~~? "test-*")

fileFilter :: String -> FindClause Bool
fileFilter fileName = (SFF.fileType ==? RegularFile) &&? (SFF.fileName ~~? fileName ||? SFF.fileName ~~? fileName ++ exts)
  where
    exts = Prelude.concat [ "(", intercalate "|" compressedFilesHandled, ")" ]


checkTestDirectories :: [FilePath] -> IO ()
checkTestDirectories [] = throwM NoTestDirectories
checkTestDirectories directories = mapM_ checkTestDirectory directories

checkTestDirectory :: FilePath -> IO ()
checkTestDirectory directoryPath = do
  inputFiles <- findInputFiles directoryPath
  when (null inputFiles) $ throw $ NoInputFile inputFile
  when (length inputFiles > 1) $ throw $ TooManyInputFiles inputFiles
  checkCorrectFile $ head inputFiles

  expectedFiles <- findExpectedFiles directoryPath
  when (null expectedFiles) $ throw $ NoExpectedFile expectedFile
  when (length expectedFiles > 1) $ throw $ TooManyExpectedFiles expectedFiles
  checkCorrectFile $ head expectedFiles

  outputFiles <- findOutputFiles directoryPath
  unless (null outputFiles) $ throw $ OutputFileDetected outputFiles
  where
    inputFile = directoryPath </> defaultInputFile
    expectedFile = directoryPath </> defaultExpectedFile

runOnTest :: GEvalSpecification -> FilePath -> IO ()
runOnTest spec testPath = do
  [expectedFile] <- findExpectedFiles testPath
  let testName = takeFileName testPath
  let modifiedSpec = spec {
        gesExpectedDirectory = Just (takeDirectory testPath),
        gesTestName = testName
        }

  (flip mapM_) (gesMetrics spec) $ \metric -> do
    withSystemTempDirectory "geval-validation" $ \tmpDir -> do
      let tmpOutDir = tmpDir </> testName
      let tmpOutFile = tmpOutDir </> defaultOutFile
      createDirectory tmpOutDir
      let specificSpec = modifiedSpec {
            gesMetrics = [metric],
            gesOutDirectory = tmpDir }
      createPerfectOutputFromExpected metric expectedFile tmpOutFile
      [(_, [MetricOutput value _])] <- geval specificSpec
      let bestValue = bestPossibleValue metric
      unless (bestValue =~ value) $ throw $ BestPossibleValueNotObtainedWithExpectedData bestValue value
      return ()
