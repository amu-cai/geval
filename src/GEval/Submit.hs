module GEval.Submit (
  submit,

  tokenFileName,
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

import Data.List (isPrefixOf)

import Network.HTTP.Simple
import Network.URI.Encode (encode)
import qualified Data.ByteString.Char8 as BS

tokenFileName :: String
tokenFileName = ".token"

submit :: Maybe String -> Maybe String -> IO ()
submit Nothing _ = failWith "Please provide a Gonito host with --gonito-host option"
submit (Just host) tok = do
  branch <- getCurrentBranch
  challengeId <- getChallengeId
  repoUrl <- getRemoteUrl "origin"
  if branch == "master" || branch == "dont-peek" then
    failWith $  "Run on prohibited branch " ++ branch
  else do
    checkEverythingCommitted
    checkRemoteSynced
    token <- getToken tok
    trigger host token branch challengeId repoUrl

trigger :: String -> String -> String -> String -> String -> IO ()
trigger host token branch challengeId repoUrl = do
    putStrLn $ "triggering: " ++ url
    req <- parseRequest url
    let params = map (\(pname, pval) -> (BS.pack $ pname, BS.pack $ pval)) [
          ("challenge", challengeId),
          ("branch", branch),
          ("token", token),
          ("url", repoUrl)]
    let req' = setRequestBodyURLEncoded params req
    httpBS req' >>= BS.putStrLn . getResponseBody
  where url = "POST " ++ hostWithProtocol ++ "/trigger-remotely"
        hostWithProtocol = if ("http://" `isPrefixOf` host) || ("https://" `isPrefixOf` host)
                           then host
                           else ("http://" ++ host)

getToken :: Maybe String -> IO String
getToken (Just token) = do
  dotToken <- readToken
  case dotToken of
    Just token' -> do
      if token /= token' then do
        putStrLn ("WARNING: Token found in " ++ tokenFileName ++ " file is different then one specified through commandline. Overwrite? [Y/N]:")
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
    ExitFailure _ -> failWith "Uncommitted changes. Please commit, push and re-run submitting."

checkRemoteSynced :: IO ()
checkRemoteSynced = do
  (_, _, _, pr) <- createProcess (shell "git fetch")
  waitForProcess pr
  localHash <- runCommand "git rev-parse HEAD"
  mRemoteTrackingBranch <- getRemoteTrackingBranch
  case mRemoteTrackingBranch of
    Just remoteTrackingBranch -> do
      remoteHash <- runCommand $ "git rev-parse " ++ remoteTrackingBranch
      if localHash == remoteHash then
        return ()
      else
        failWith "Changes are not merged with remote branch."
    Nothing -> failWith "No tracking branch found, use `git push -u origin BRANCH_NAME`"

getCurrentBranch :: IO String
getCurrentBranch = runCommand "git rev-parse --abbrev-ref HEAD"

getRemoteTrackingBranch :: IO (Maybe String)
getRemoteTrackingBranch = do
  (code, content, _) <- readCreateProcessWithExitCode (shell "git rev-parse --abbrev-ref --symbolic-full-name @{u}") ""
  case code of
    ExitSuccess -> return $ Just $ takeWhile (/= '\n') content
    ExitFailure _ -> return Nothing

getRemoteUrl :: String -> IO String
getRemoteUrl remote = runCommand $ "git config --get remote." ++ remote ++ ".url"

getChallengeId :: IO String
getChallengeId = getCurrentDirectory >>= return . takeBaseName

getRepoRoot :: IO String
getRepoRoot = runCommand "git rev-parse --show-toplevel"

tokenFilePath :: IO String
tokenFilePath = (++ "/" ++ tokenFileName ) <$> getRepoRoot

readToken :: IO (Maybe String)
readToken = do
  file <- tokenFilePath
  catchIOError
    (Just . takeWhile (/= '\n') <$> readFile file)
    (\e -> if isDoesNotExistError e
           then return Nothing
           else failWith ("Unable to read " ++ tokenFileName ++ " file") >> return Nothing
    )

writeToken :: String -> IO ()
writeToken token = do
  file <- tokenFilePath
  catchIOError (writeFile file token) (\_ -> failWith $ "Unable to write " ++ tokenFileName ++ " file")

runCommand :: String -> IO String
runCommand cmd = do
  content <- readCreateProcess (shell cmd) ""
  return $ takeWhile (/= '\n') content

failWith :: String -> IO ()
failWith msg = hPutStrLn stderr ("ERROR: " ++ msg) >> exitFailure
