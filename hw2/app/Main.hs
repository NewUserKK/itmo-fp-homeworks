{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.NonEmpty as NE hiding (sort)
import FilesystemCore (FileSystem, FSState(..), CommandExecutionError, toAbsoluteFSPath, getFileByPathOrError)
import FilesystemCommands
import CVSCommands
import FilesystemLoader
import FilesystemDumper
import Options.Applicative hiding (command)
import System.IO (hFlush, stdout)
import Utils
import Path
import File
import Data.List
import CVSCore (CommitInfo, MergeStrategy(..), commitRealFilePath)

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
  | Exit
  | ShowHelp

data UnknownCommandError
  = UnknownCommandError String
  | IntParseError String
  | UnknownMergeStrategy String
  deriving (Show)

{-|
  The main entry point for the program.
  Takes path to root directory from where to launch file manager as required argument.
-}
main :: IO ()
main = do
  launch =<< execParser opts
    where
      opts = info (parseArguments <**> helper)
        ( fullDesc
        <> progDesc "Run file manager on ROOT directory"
        <> header "Command-line file manager with version control system"
        )

{-|
  Load filesystem in memory, execute user commands and dump resulting filesystem to the
  real PC filesystem.
-}
launch :: Arguments -> IO ()
launch Arguments{ workingDirectory = root } = do
  initialState <- loadFilesystem root
  resultState <- execStateT prompt initialState
  dumpFilesystem resultState


{-|
  Endless loop waiting for user to prompt and executing given commands
-}
prompt :: FileSystem ()
prompt = do
  currentDir <- gets currentDirectory
  liftIO $ putStr $ show currentDir ++ "> "
  liftIO $ hFlush stdout
  line <- liftIO $ getLine
  let command = parseCommand line
  case command of
    Right cmd -> execCommand cmd `catch` printCommandExecutionError
    Left err -> printError err
  case command of
    Right Exit -> return ()
    _ -> prompt

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
    "exit" :| [] -> Right Exit
    "help" :| [] -> Right ShowHelp
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
    Exit -> execExit
    ShowHelp -> execShowHelp
    
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

execExit :: FileSystem ()
execExit = do
  liftIO $ putStrLn "Dumping files on hard drive..."
  liftIO $ putStrLn "Backup will be created in .old_fs directory"

execShowHelp :: FileSystem ()
execShowHelp = liftIO $ putStrLn getHelp

getRevisionsMessageForDocument :: [CommitInfo] -> String
getRevisionsMessageForDocument [] = "No history"
getRevisionsMessageForDocument revisions = do
  let path = pathToString $ commitRealFilePath (Prelude.head revisions)
  "Commit history for file" ++ path ++ "\n" ++
    intercalate "\n" (Prelude.map show $ sort revisions)

printCommandExecutionError :: CommandExecutionError -> FileSystem ()
printCommandExecutionError e = liftIO $ print e

printError :: UnknownCommandError -> FileSystem ()
printError err = liftIO $ putStrLn $ "Wrong command: " ++ show err ++ ". Usage: help"

tryParse :: String -> Either UnknownCommandError Int
tryParse s =
  case readMaybeInt s of
    Just i -> Right i
    Nothing -> Left $ IntParseError s

getHelp :: String
getHelp = intercalate "\n"
  [ "`cd <path>` -- перейти в указанный путь;"
  , "`ls <path>` -- показать содержимое текущей директории;"
  , "`mkdir <path>` -- создать директорию, включая промежуточные;"
  , "`touch <path> <text>` -- создать файл, включая промежуточные директории;"
  , "`cat <path>` -- отобразить содержимое файла;"
  , "`rm <path>` -- удалить папку/файл;"
  , "`cp <path> <target-dir-path>` -- скопировать папку/файл в указанную директорию с созданием промежуточных;"
  , "`append-file <path> <text>` -- записать в файл текстовую информацию;"
  , "`find <path> <name>` -- поиск файла по названию в указанной директории и ее подчастях и вывод пути до файла;"
  , "`info <path>` -- отобразить информацию о заданном файле;"
  , "`info <path>` -- отобразить информацию о директории;"
  , "`cvs init <path>` -- возможность инициализации системы контроля версий (СКВ) в указанной директории;"
  , "если текущая директория является частью уже инициализированной в СКВ, то инициализация не требуется;"
  , "`cvs add <path>` -- добавление файла или папки (всех файлов внутрии нее) в СКВ;"
  , "`cvs update <path> <comment>` добавление измененной версии файла и автоматическое создание новой ревизии данного файла;"
  , "`cvs history <path-to-file>` -- просмотр истории изменений файла;"
  , "`cvs history <path-to-dir>` -- просмотр упорядоченной историю изменений файлов в заданной директории и ее поддиректориях;"
  , "`cvs show <file> <index>` -- вывод конкретной версии файла по индексу в истории его изменений;"
  , "`cvs merge <file> <index-1> <index-2> <strategy>` -- объединение разных ревизий одного файла;"
  , "`cvs rm <path>` -- удалить файл из СКВ;"
  , "`cvs rm-rev <path> <index>` -- удалить ревизию файла;"
  , "`exit` -- завершить работу с менеджером и выгрузить изменения на диск;"
  , "`help` -- показать это сообщение."
  ]
