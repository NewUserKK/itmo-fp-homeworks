{-# LANGUAGE LambdaCase #-}
module FilesystemLoader where

import System.Directory
import Filesystem
import Data.Maybe (fromJust, catMaybes)

loadFilesystem :: String -> IO FSState
loadFilesystem rootPath = do
  fsRoot <- getCurrentDirectory
  root <- fromJust <$> constructDirectory fsRoot rootPath
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
    catMaybes <$> traverse (fileFromPath currentDir) paths

fileFromPath :: FilePath -> FilePath -> IO (Maybe File)
fileFromPath parentPath path = do
  doc <- constructDocument path
  case doc of
    Nothing -> constructDirectory parentPath path
    document@(Just _) -> return document

constructDirectory :: FilePath -> FilePath -> IO (Maybe File)
constructDirectory parentPath path = do
  isDirectory <- doesDirectoryExist path
  if not isDirectory
    then return Nothing
    else do
      contents <- getFolderContents parentPath path
      return $ Just Directory {
          filePath = stringToPath (parentPath </> path)
        , fileAccessibility = ""
        , directoryContents = contents
        , directoryParent = Nothing
        }

constructDocument :: FilePath -> IO (Maybe File)
constructDocument path = do
  isFile <- doesFileExist path
  if not isFile
    then return Nothing
    else return $ Just Document
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
