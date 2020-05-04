{-# LANGUAGE LambdaCase #-}

module FilesystemLoader where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Maybe (catMaybes, fromJust)
import Filesystem
import System.Directory

loadFilesystem :: String -> IO FSState
loadFilesystem rootPath = do
  fsRoot <- getCurrentDirectory
  root <- fromJust <$> (runMaybeT $ constructDirectory fsRoot rootPath)
  let rootWithParents = updateParents root
  return FSState
    { currentDirectory = rootWithParents
    , rootDirectory = rootWithParents
    }

getFolderContents :: FilePath -> FilePath -> IO [File]
getFolderContents parentPath path = do
  withCurrentDirectory (parentPath </> path) $ do
    currentDir <- getCurrentDirectory
    paths <- listDirectory currentDir
    catMaybes <$> traverse (runMaybeT . fileFromPath currentDir) paths

fileFromPath :: FilePath -> FilePath -> MaybeT IO (File)
fileFromPath parentPath path = (constructDocument path) <|> (constructDirectory parentPath path)

constructDirectory :: FilePath -> FilePath -> MaybeT IO (File)
constructDirectory parentPath path = do
  isDirectory <- liftIO $ doesDirectoryExist path
  guard isDirectory
  contents <- liftIO $ getFolderContents parentPath path
  return Directory
    { filePath = stringToPath (parentPath </> path)
    , fileAccessibility = ""
    , directoryContents = contents
    , directoryParent = Nothing
    }

constructDocument :: FilePath -> MaybeT IO (File)
constructDocument path = do
  isFile <- liftIO $ doesFileExist path
  guard isFile
  return Document
    { filePath = stringToPath path
    , fileAccessibility = ""
    , documentExtension = "ext"
    , documentCreationTime = ""
    , documentUpdateTime = ""
    , documentSize = 42
    }

updateParents :: File -> File
updateParents root@(Directory{}) = do
  let updateFun =
        \case
            d@Directory{} -> d { directoryParent = Just root }
            file -> file
  let contents = map updateFun $ directoryContents root
  let updRoot = root { directoryContents = contents }
  let contentsWithParent = map updateParents (directoryContents updRoot)
  updRoot { directoryContents = contentsWithParent }
updateParents doc = doc

concatPath :: FilePath -> FilePath -> FilePath
concatPath parentPath path = parentPath ++ "/" ++ path

(</>) ::  FilePath -> FilePath -> FilePath
(</>) = concatPath
