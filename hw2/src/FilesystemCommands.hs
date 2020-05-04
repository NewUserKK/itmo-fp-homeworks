module FilesystemCommands where

import Control.Monad.Catch (throwM)
import Control.Monad.State
import Data.List.NonEmpty as NE
import Filesystem

changeDirectory :: String -> FileSystem ()
changeDirectory path = do
  st <- get
  evalled <- liftIO $ evalStateT (getDirectoryByPath path) st
  modify (\s -> s {currentDirectory = evalled})

listContents :: String -> FileSystem [String]
listContents path = do
  dir <- getDirectoryByPath path
  return $ (Prelude.map nameByPath) $ directoryContents dir

makeDirectory :: String -> FileSystem ()
makeDirectory path = do
  root <- gets rootDirectory
  let e = mkdir (stringToPath path) root
  case e of
    Left t -> throwM t
    Right new -> modify (\s -> s {rootDirectory = new})

mkdir :: Path -> File -> Either CommandExecutionError File
mkdir path@(x :| next : xs) root =
  case findInFolder root x of
    Just dir@(Directory {}) -> mkdir (next :| xs) dir
    Just (Document {}) -> Left DirectoryIsDocument
    Nothing -> mkdir path (root {
      directoryContents = directoryContents root ++ [constructDirectoryRelative root x]
    })
mkdir (x :| []) root =
  case findInFolder root x of
    Just _ -> Left FileAlreadyExists
    Nothing -> Right $ (root {
      directoryContents = directoryContents root ++ [constructDirectoryRelative root x]
    })
