{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
  fsRoot <- makeAbsolute rootPath
  print $ fsRoot
  root <- fromJust <$> (runMaybeT $ constructDirectory fsRoot "")
  return FSState
    { currentDirectory = root
    , rootDirectory = root
    , realRootPath = fsRoot
    }

getFolderContents :: FilePath -> IO [File]
getFolderContents path = do
  withCurrentDirectory path $ do
    currentDir <- getCurrentDirectory
    paths <- listDirectory currentDir
    catMaybes <$> traverse (runMaybeT . fileFromPath currentDir) paths

fileFromPath :: FilePath -> FilePath -> MaybeT IO (File)
fileFromPath parentPath path = (constructDocument path) <|> (constructDirectory parentPath path)

constructDirectory :: FilePath -> FilePath -> MaybeT IO (File)
constructDirectory parentPath path = do
  let realPath = parentPath </> path
  let localPath = stringToPath $ if path == "" then "/" else path
  isDirectory <- liftIO $ doesDirectoryExist realPath
  guard isDirectory
  contents <- liftIO $ getFolderContents realPath
  let updateFunction = \file ->
       case file of
          d@Directory{} -> d { directoryParent = Just localPath }
          Document{} -> file
  liftIO $ print $ "===========" 
  liftIO $ print $ realPath 
  liftIO $ print $ map (directoryParent . updateFunction) $ filter (\case Directory{} -> True; _ -> False) contents
  return Directory
    { filePath = localPath
    , fileAccessibility = ""
    , directoryContents = map updateFunction contents
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
    , documentContent = "content"
    }

concatPath :: FilePath -> FilePath -> FilePath
concatPath "" path = path
concatPath parentPath "" = parentPath
concatPath parentPath path = parentPath ++ "/" ++ path

(</>) ::  FilePath -> FilePath -> FilePath
(</>) = concatPath
