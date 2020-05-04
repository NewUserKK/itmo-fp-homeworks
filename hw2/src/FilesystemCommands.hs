{-# LANGUAGE OverloadedStrings #-}

module FilesystemCommands where

import Control.Monad.State
import Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy as BS
import Filesystem
import Control.Exception (throw)

changeDirectory :: StringPath -> FileSystem ()
changeDirectory path = do
  st <- get
  newDirectory <- liftIO $ evalStateT (getDirectoryByPath path) st
  modify (\s -> s { currentDirectory = newDirectory })

listContents :: StringPath -> FileSystem [String]
listContents path = do
  dir <- getDirectoryByPath path
  return $ (Prelude.map nameByPath) $ directoryContents dir

makeDirectory :: StringPath -> FileSystem ()
makeDirectory path = do
  root <- gets rootDirectory
  modify (\s -> s { rootDirectory = mkdir (stringToPath path) root })

mkdir :: Path -> File -> File
mkdir = undefined
--mkdir path@(x :| next : xs) root =
--  case findInFolder root x of
--    Just dir@(Directory {}) -> mkdir (next :| xs) dir
--    Just (Document {}) -> throw DirectoryExpected
--    Nothing -> mkdir path (root {
--      directoryContents = directoryContents root ++ [constructDirectoryRelative root x]
--    })
--mkdir (x :| []) root =
--  case findInFolder root x of
--    Just _ -> throw FileAlreadyExists
--    Nothing -> root {
--      directoryContents = directoryContents root ++ [constructDirectoryRelative root x]
--    }

fileContents :: StringPath -> FileSystem BS.ByteString
fileContents path = do
  file <- getDocumentByPath path
  undefined