{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.NonEmpty as NE
import Filesystem (FileSystem, FSState(..), CommandExecutionError)
import FilesystemCommands
import FilesystemLoader
import Options.Applicative
import System.IO (hFlush, stdout)
import Utils
import Path
import File

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

data UnknownCommandError
  = UnknownCommandError String
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
    _ -> Left $ UnknownCommandError s

execCommand :: Command -> FileSystem ()
execCommand e =
  case e of
    ChangeDirectory path -> execCD path
    ListFiles path -> execLs path
    MakeDirectory path -> execMkdir path
    MakeDocument path text -> execTouch path text
    RemoveFile path -> execRm path
    Copy from to -> execCp from to
    ReadFile path -> execReadFile path
    AppendFile path text -> execAppendFile path text
    PrintInfo path -> execPrintInfo path
    Find path name -> execFind path name

execCD :: StringPath -> FileSystem ()
execCD path = do
  changeDirectory path
  newState <- get
  liftIO $ print $ filePath . currentDirectory $ newState

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

printCommandExecutionError :: CommandExecutionError -> FileSystem ()
printCommandExecutionError e = liftIO $ print e

printError :: UnknownCommandError -> FileSystem ()
printError err = liftIO $ putStrLn $ "Wrong command: " ++ show err
