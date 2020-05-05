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
  root <- fromJust <$> (runMaybeT $ constructDirectory fsRoot "" "/")
  return FSState
    { currentDirectory = root
    , rootDirectory = root
    , realRootPath = fsRoot
    }


getFolderContents :: FilePath -> FilePath -> FilePath -> IO [File]
getFolderContents realFsRoot realPath localPath = do
  withCurrentDirectory realPath $ do
    currentDir <- getCurrentDirectory
    paths <- listDirectory currentDir
--    liftIO $ print $ paths
--    liftIO $ print $ "getFolderContents currentDir: " ++ realPath
--    liftIO $ print $ "getFolderContents localPath: " ++ localPath
    catMaybes <$> traverse (runMaybeT . fileFromPath realFsRoot localPath) paths


fileFromPath :: FilePath -> FilePath -> FilePath -> MaybeT IO (File)
fileFromPath realFsRoot parentPath path =
  (constructDocument parentPath path) <|> (constructDirectory realFsRoot parentPath path)


constructDirectory :: FilePath -> FilePath -> FilePath -> MaybeT IO (File)
constructDirectory realFsRoot localParentPath name = do
  let localPath = localParentPath </> name
  let realPath = realFsRoot ++ localPath

--  liftIO $ print $ "constructDirectory name: " ++ name
--  liftIO $ print $ "constructDirectory localPath: " ++ localPath
--  liftIO $ print $ "constructDirectory realPath: " ++ realPath

  isDirectory <- liftIO $ doesDirectoryExist realPath
  guard isDirectory

  let updateFunction = \file ->
       case file of
          dir@Directory{} -> dir { directoryParent = Just $ stringToPath localPath }
          Document{} -> file
  contents <- liftIO $ (map updateFunction) <$> getFolderContents realFsRoot realPath localPath
  permissions <- liftIO $ getPermissions realPath

  return Directory
    { filePath = stringToPath localPath
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
