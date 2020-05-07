{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.NonEmpty as NE
import Filesystem (FileSystem, FSState(..), CommandExecutionError, toAbsoluteFSPath, getFileByPathOrError)
import FilesystemCommands
import CVSCommands
import FilesystemLoader
import Options.Applicative
import System.IO (hFlush, stdout)
import Utils
import Path
import File
import Data.List (intercalate)
import CVS (CommitInfo, MergeStrategy(..), commitRealFilePath)

data Arguments =
  Arguments
    { workingDirectory :: String
    }

data Command
  = ChangeDirectory StringPath
  | ListFiles StringPath
  | MakeDirectory StringPath
  | MakeDocument StringPath BS.ByteString
  | RemoveFile StringPath
  | ReadFile StringPath
  | AppendFile StringPath BS.ByteString
  | Copy StringPath StringPath
  | PrintInfo StringPath
  | Find StringPath String
  | CVSInit StringPath
  | CVSAdd StringPath
  | CVSUpdate StringPath String
  | CVSHistory StringPath
  | CVSShow StringPath Int
  | CVSRemove StringPath
  | CVSRemoveRevision StringPath Int
  | CVSMergeRevisions StringPath Int Int MergeStrategy

data UnknownCommandError
  = UnknownCommandError String
  | IntParseError String
  | UnknownMergeStrategy String
  deriving (Show)

main :: IO ()
main = do
  launch =<< execParser opts
    where
      opts = info (parseArguments <**> helper)
        ( fullDesc
        <> progDesc "Print a greeting for TARGET"
        <> header "hello - a test for optparse-applicative"
        )

launch :: Arguments -> IO ()
launch Arguments{ workingDirectory = root } = do
  initialState <- loadFilesystem root
  evalStateT loop initialState

loop :: FileSystem ()
loop = do
  currentDir <- gets currentDirectory
  liftIO $ putStr $ show currentDir ++ "> "
  liftIO $ hFlush stdout
  line <- liftIO $ getLine
  case parseCommand line of
    Right cmd -> execCommand cmd `catch` printCommandExecutionError
    Left err -> printError err
  loop

parseArguments :: Parser Arguments
parseArguments =
  Arguments <$> argument str (metavar "ROOT" <> help "Root directory for file manager")

parseCommand :: String -> Either UnknownCommandError Command
parseCommand s =
  case (splitOn ' ' s) of
    "cd" :| [path] -> Right $ ChangeDirectory path
    "ls" :| [path] -> Right $ ListFiles path
    "mkdir" :| [path] -> Right $ MakeDirectory path
    "rm" :| [path] -> Right $ RemoveFile path
    "cp" :| from : [to] -> Right $ Copy from to
    "touch" :| path : [text] -> Right $ MakeDocument path $ BS.pack text
    "touch" :| [path] -> Right $ MakeDocument path ""
    "append-file" :| path : [text] -> Right $ AppendFile path $ BS.pack text
    "cat" :| [path] -> Right $ ReadFile path
    "info" :| [path] -> Right $ PrintInfo path
    "find" :| path : [name] -> Right $ Find path name
    "cvs" :| ["init"] -> Right $ CVSInit "."
    "cvs" :| "init" : [path] -> Right $ CVSInit path
    "cvs" :| "add" : [path] -> Right $ CVSAdd path
    "cvs" :| "update" : path : [comment] -> Right $ CVSUpdate path comment
    "cvs" :| "history" : [path] -> Right $ CVSHistory path
    "cvs" :| "show" : path : [index] -> tryParse index >>= return . (CVSShow path)
    "cvs" :| "rm-rev" : path : [index] -> tryParse index >>= return . (CVSRemoveRevision path)
    "cvs" :| "rm" : [path] -> Right $ CVSRemove path
    "cvs" :| "merge" : path : index1 : index2 : [strategy] -> do
      i1 <- tryParse index1
      i2 <- tryParse index2
      case strategy of
        "left" -> Right $ CVSMergeRevisions path i1 i2 MergeLeft
        "right" -> Right $ CVSMergeRevisions path i1 i2 MergeRight
        "both" -> Right $ CVSMergeRevisions path i1 i2 MergeBoth
        _ -> Left $ UnknownMergeStrategy strategy
    _ -> Left $ UnknownCommandError s

execCommand :: Command -> FileSystem ()
execCommand e =
  case e of
    ChangeDirectory path -> execCd path
    ListFiles path -> execLs path
    MakeDirectory path -> execMkdir path
    MakeDocument path text -> execTouch path text
    RemoveFile path -> execRm path
    Copy from to -> execCp from to
    ReadFile path -> execReadFile path
    AppendFile path text -> execAppendFile path text
    PrintInfo path -> execPrintInfo path
    Find path name -> execFind path name
    CVSInit path -> execCvsInit path
    CVSAdd path -> execCvsAdd path
    CVSUpdate path comment -> execCvsUpdate path comment
    CVSHistory path -> execCvsHistory path
    CVSShow path index -> execCvsShow path index
    CVSRemove path -> execCvsRemove path 
    CVSRemoveRevision path index -> execCvsRemoveRevision path index
    CVSMergeRevisions path i1 i2 strategy -> execCvsMerge path i1 i2 strategy
    
execCd :: StringPath -> FileSystem ()
execCd path = changeDirectory path

execLs :: StringPath -> FileSystem ()
execLs path = do
  contents <- FilesystemCommands.getContents path
  liftIO $ print $ (Prelude.map fileName) contents

execMkdir :: StringPath -> FileSystem ()
execMkdir = makeDirectory

execTouch :: StringPath -> BS.ByteString -> FileSystem ()
execTouch = makeFile

execRm :: StringPath -> FileSystem ()
execRm = removeFile

execCp :: StringPath -> StringPath -> FileSystem ()
execCp = copyFile

execReadFile :: StringPath -> FileSystem ()
execReadFile path = do
  contents <- readFileContents path
  liftIO $ BS.putStrLn contents

execAppendFile :: StringPath -> BS.ByteString -> FileSystem ()
execAppendFile = appendToFile

execPrintInfo :: StringPath -> FileSystem ()
execPrintInfo path = getFileInfo path >>= liftIO . putStrLn

execFind :: StringPath -> String -> FileSystem ()
execFind path name = do
  result <- findByName path name
  liftIO $ print result

execCvsInit :: StringPath -> FileSystem ()
execCvsInit path = do
  created <- cvsInit path
  liftIO $ putStrLn $ "Initialized empty CVS repository at " ++ (pathToString $ filePath created)

execCvsAdd :: StringPath -> FileSystem ()
execCvsAdd = cvsAdd

execCvsUpdate :: StringPath -> String -> FileSystem ()
execCvsUpdate = cvsUpdate

execCvsHistory :: StringPath -> FileSystem ()
execCvsHistory stringPath = do
  path <- toAbsoluteFSPath $ stringToPath stringPath
  file <- getFileByPathOrError path
  case file of
    Document{} -> do
      message <- getRevisionsMessageForDocument <$> cvsHistoryForDocument file
      liftIO $ putStrLn message
    Directory{} -> do
      history <- cvsHistoryForDirectory file
      liftIO $ putStrLn $ "Commit history for directory " ++ stringPath
      liftIO $ putStrLn $ intercalate "\n" (Prelude.map getRevisionsMessageForDocument history)

execCvsShow :: StringPath -> Int -> FileSystem ()
execCvsShow path index = do
  contents <- cvsShow path index
  liftIO $ BS.putStrLn contents
  
execCvsRemove :: StringPath -> FileSystem ()
execCvsRemove path = do
  cvsRemove path
  liftIO $ putStrLn $ "Removed " ++ path ++ " from CVS"
  
execCvsRemoveRevision :: StringPath -> Int -> FileSystem ()
execCvsRemoveRevision path index = do
  cvsRemoveRevision path index
  liftIO $ putStrLn $ "Removed revision " ++ show index ++ " of " ++ path ++ " from CVS"

execCvsMerge :: StringPath -> Int -> Int -> MergeStrategy -> FileSystem ()
execCvsMerge path index1 index2 strategy = do
  _ <- cvsMergeRevisions path index1 index2 strategy
  liftIO $ putStrLn "Created new file revision"

getRevisionsMessageForDocument :: [CommitInfo] -> String
getRevisionsMessageForDocument [] = "No history"
getRevisionsMessageForDocument revisions = do
  let path = pathToString $ commitRealFilePath (Prelude.head revisions)
  "Commit history for file" ++ path ++ "\n" ++ intercalate "\n" (Prelude.map show revisions)

printCommandExecutionError :: CommandExecutionError -> FileSystem ()
printCommandExecutionError e = liftIO $ print e

printError :: UnknownCommandError -> FileSystem ()
printError err = liftIO $ putStrLn $ "Wrong command: " ++ show err

tryParse :: String -> Either UnknownCommandError Int
tryParse s =
  case readMaybeInt s of
    Just i -> Right i
    Nothing -> Left $ IntParseError s