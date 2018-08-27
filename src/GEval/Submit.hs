module GEval.Submit (
  submit,

  getToken,
  readToken,
  writeToken,
  tokenFilePath,

  getCurrentBranch,
  getChallengeId,
  getRepoRoot,
  getRemoteUrl,

  checkEverythingCommitted,
  checkRemoteSynced
) where

import Data.Char (toLower)

import System.Process hiding (runCommand)
import System.IO
import System.IO.Error
import System.Exit
import System.Directory
import System.FilePath

import Network.HTTP.Simple
import Network.URI.Encode (encode)
import qualified Data.ByteString.Char8 as BS

submit :: Maybe String -> Maybe String -> IO ()
submit Nothing _ = failWith "ERROR: Please provide --gonito-host"
submit (Just host) tok = do
  token <- getToken tok
  branch <- getCurrentBranch
  challengeId <- getChallengeId
  repoUrl <- getRemoteUrl "origin"
  if branch == "master" || branch == "dont-peek" then
    failWith $  "ERROR: Run on prohibited branch " ++ branch
  else do
    checkEverythingCommitted
    checkRemoteSynced
    trigger host token branch challengeId repoUrl

trigger :: String -> String -> String -> String -> String -> IO ()
trigger host token branch challengeId repoUrl = do
    req <- parseRequest url
    httpBS req >>= BS.putStrLn . getResponseBody
  where url = "POST http://" ++ host ++ "/trigger-remotely?token=" ++ (encode token) ++ "&branch=" ++ (encode branch) ++ "&challenge=" ++ (encode challengeId) ++ "&url=" ++ (encode repoUrl)

getToken :: Maybe String -> IO String
getToken (Just token) = do
  dotToken <- readToken
  case dotToken of
    Just token' -> do
      if token /= token' then do
        putStrLn "WARNING: Token found in .token file is different then one specified through commandline. Overwrite? [Y/N]:"
        answer <- getChar
        case toLower answer of
          'y' -> writeToken token
          'n' -> return ()
      else return ()
    Nothing -> writeToken token
  return token
getToken Nothing = do
  dotToken <- readToken
  case dotToken of
    Just token -> return token
    Nothing -> do
      putStrLn "WARNING: No token specified. Please provide your token below:"
      token <- getLine
      writeToken token
      return token

checkEverythingCommitted :: IO ()
checkEverythingCommitted = do
  callCommand "git update-index -q --ignore-submodules --refresh"
  (code, _, _) <- readCreateProcessWithExitCode (shell "git diff-index --quiet HEAD --") ""
  case code of
    ExitSuccess -> return ()
    ExitFailure _ -> failWith "ERROR: Uncommitted changes."

checkRemoteSynced :: IO ()
checkRemoteSynced = do
  (_, _, _, pr) <- createProcess (shell "git fetch")
  waitForProcess pr
  localHash <- runCommand "git rev-parse HEAD"
  remoteTrackingBranch <- runCommand "git rev-parse --abbrev-ref --symbolic-full-name @{u}"
  remoteHash <- runCommand $ "git rev-parse " ++ remoteTrackingBranch
  if localHash == remoteHash then
    return ()
  else
    failWith "ERROR: Changes are not merged with remote branch."

getCurrentBranch :: IO String
getCurrentBranch = runCommand "git rev-parse --abbrev-ref HEAD"

getRemoteUrl :: String -> IO String
getRemoteUrl remote = runCommand $ "git config --get remote." ++ remote ++ ".url"

getChallengeId :: IO String
getChallengeId = getCurrentDirectory >>= return . takeBaseName

getRepoRoot :: IO String
getRepoRoot = runCommand "git rev-parse --show-toplevel"

tokenFilePath :: IO String
tokenFilePath = (++ "/.token" ) <$> getRepoRoot

readToken :: IO (Maybe String)
readToken = do
  file <- tokenFilePath
  catchIOError
    (Just . takeWhile (/= '\n') <$> readFile file)
    (\e -> if isDoesNotExistError e
           then return Nothing
           else failWith "ERROR: Unable to read .token file" >> return Nothing
    )

writeToken :: String -> IO ()
writeToken token = do
  file <- tokenFilePath
  catchIOError (writeFile file token) (\_ -> failWith "ERROR: Unable to write .token file")

runCommand :: String -> IO String
runCommand cmd = do
  content <- readCreateProcess (shell cmd) ""
  return $ takeWhile (/= '\n') content

failWith :: String -> IO ()
failWith msg = hPutStrLn stderr msg >> exitFailure
