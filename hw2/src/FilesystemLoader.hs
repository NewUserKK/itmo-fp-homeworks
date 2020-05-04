{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module FilesystemLoader where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy as BS
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
fileFromPath parentPath path = 
  (constructDocument parentPath path) <|> (constructDirectory parentPath path)


constructDirectory :: FilePath -> FilePath -> MaybeT IO (File)
constructDirectory parentPath path = do
  let realPath = parentPath </> path
  let localPath = stringToPath $ if path == "" then "/" else path
  isDirectory <- liftIO $ doesDirectoryExist realPath

  guard isDirectory

  let updateFunction = \file ->
       case file of
          d@Directory{} -> d { directoryParent = Just localPath }
          Document{} -> file
  contents <- liftIO $ (map updateFunction) <$> getFolderContents realPath
  permissions <- liftIO $ getPermissions path

  return Directory
    { filePath = localPath
    , filePermissions = permissions
    , directoryContents = contents
    , directoryParent = Nothing
    }


constructDocument :: FilePath -> FilePath -> MaybeT IO (File)
constructDocument parentPath path = do
  let realPath = parentPath </> path
  isFile <- liftIO $ doesFileExist path
  guard isFile

  let filepath = stringToPath realPath
  let extension = extensionFromPath filepath
  permissions <- liftIO $ getPermissions path
--  creationTime <- liftIO $ getCreationTime path
  modificationTime <- liftIO $ getModificationTime path
  fileSize <- liftIO $ fromIntegral <$> getFileSize path
  contents <- liftIO $ BS.readFile path

  return Document
    { filePath = filepath
    , filePermissions = permissions
    , documentExtension = extension
    , documentCreationTime = modificationTime
    , documentUpdateTime = modificationTime
    , documentSize = fileSize
    , documentContent = contents
    }


concatPath :: FilePath -> FilePath -> FilePath
concatPath "" path = path
concatPath parentPath "" = parentPath
concatPath parentPath path = parentPath ++ "/" ++ path


(</>) ::  FilePath -> FilePath -> FilePath
(</>) = concatPath
