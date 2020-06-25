{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Task5.FilesystemLoader
  ( loadFilesystem
  ) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Maybe (catMaybes)
import System.Directory
import Task5.FS
import Task5.Path

{-|
  Load filesystem by root path and return constructed from it @FSState@ .
-}
loadFilesystem :: String -> IO (Maybe FS)
loadFilesystem rootPath = do
  fsRoot <- makeAbsolute rootPath
  print $ fsRoot
  runMaybeT $ constructDirectory fsRoot "" "/"

getFolderContents :: FilePath -> FilePath -> FilePath -> IO [FS]
getFolderContents realFsRoot realPath localPath = do
  withCurrentDirectory realPath $ do
    currentDir <- getCurrentDirectory
    paths <- listDirectory currentDir
    catMaybes <$> traverse (runMaybeT . fileFromPath realFsRoot localPath) paths

fileFromPath :: FilePath -> FilePath -> FilePath -> MaybeT IO FS
fileFromPath realFsRoot parentPath path =
  (constructDocument parentPath path) <|> (constructDirectory realFsRoot parentPath path)

constructDirectory :: FilePath -> FilePath -> FilePath -> MaybeT IO FS
constructDirectory realFsRoot localParentPath dirName = do
  let localPath =
        case localParentPath of
          "" -> "/"
          "/" -> "/" ++ dirName
          _ -> localParentPath </> dirName
  let realPath = realFsRoot ++ localPath

  isDirectory <- liftIO $ doesDirectoryExist realPath
  guard isDirectory

  content <- liftIO $ getFolderContents realFsRoot realPath localPath

  return Dir
    { _name = nameByPath $ stringToPath localPath
    , _contents = content
    }

constructDocument :: FilePath -> FilePath -> MaybeT IO FS
constructDocument parentPath path = do
  let realPath =
        if parentPath == "/"
          then "/" ++ path
          else parentPath </> path
  isFile <- liftIO $ doesFileExist path
  guard isFile

  let filepath = stringToPath realPath

  return File
    { _name = nameByPath filepath
    }
    