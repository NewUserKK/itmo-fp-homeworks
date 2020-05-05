{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List.NonEmpty as NE
import Filesystem
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
  | ReadFile StringPath
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
    "touch" :| path : [text] -> Right $ MakeDocument path $ BS.pack text
    "touch" :| [path] -> Right $ MakeDocument path ""
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
    ReadFile path -> execReadFile path
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
execMkdir path = makeDirectory path

execTouch :: StringPath -> BS.ByteString -> FileSystem ()
execTouch path text = makeFile path text

execReadFile :: StringPath -> FileSystem ()
execReadFile path = do
  contents <- readFileContents path
  liftIO $ BS.putStrLn contents
  
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
