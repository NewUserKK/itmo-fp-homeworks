{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Filesystem where

import Control.Exception
import Control.Monad.State
import Data.List.NonEmpty as NE
import Utils
import Path
import File
import Control.Monad.Catch (throwM)

type FileSystem a = StateT FSState IO a

data FSState =
  FSState
    { currentDirectory :: File
    , rootDirectory :: File
    , realRootPath :: StringPath
    }

data CommandExecutionError
  = DirectoryExpected
  | DocumentExpected
  | NoSuchFile
  | FileAlreadyExists
  | FileNotFound
  | FailedToCreateFile
  | CannotCreateRoot
  | CannotRemoveRoot
  | CannotRemoveParent
  | CVSAlreadyExists StringPath
  | CVSDoesNotExist
  | InvalidCVSRepository
  | InvalidCVSRevisionDirectory
  | FileNotAddedToCVS
  | MalformedCommitInfo
  deriving (Show, Exception)


toAbsoluteFSPath :: Path -> FileSystem Path
toAbsoluteFSPath path = do
  fsRoot <- filePath <$> gets rootDirectory
  currentDir <- filePath <$> gets currentDirectory
  return $ toAbsolutePath path fsRoot currentDir

getFileByPathOrError :: Path -> FileSystem File
getFileByPathOrError path = do
  maybeFile <- getFileByPath path
  case maybeFile of
    Just f -> return f
    Nothing -> throwM NoSuchFile

getFileByPath :: Path -> FileSystem (Maybe File)
getFileByPath path = toAbsoluteFSPath path >>= getFileByAbsolutePath

getFileByAbsolutePath :: Path -> FileSystem (Maybe File)
getFileByAbsolutePath path = do
  root <- gets rootDirectory
  case path of
    ("/" :| []) -> return $ Just root
    ("/" :| remaining) -> getFileByRelativePath (NE.fromList remaining) root
    relativePath -> getFileByRelativePath relativePath root

getFileByRelativePath :: Path -> File -> FileSystem (Maybe File)
getFileByRelativePath _ Document{} = throw DirectoryExpected
getFileByRelativePath absPath@("/" :| _) _ = getFileByAbsolutePath absPath
getFileByRelativePath (x :| []) root = Just <$> moveNext root x
getFileByRelativePath (x :| next : xs) root = moveNext root x >>= getFileByRelativePath (next :| xs)

moveNext :: File -> String -> FileSystem File
moveNext root name =
  case findInFolder root name of
    Just f -> return f
    Nothing -> throwM NoSuchFile

getDirectoryByPath :: Path -> FileSystem File
getDirectoryByPath path = do
  file <- getFileByPath path
  case file of
    Just dir@Directory{} -> return dir
    Just Document{} -> throwM DirectoryExpected
    Nothing -> throwM NoSuchFile

getDocumentByPath :: Path -> FileSystem File
getDocumentByPath path = do
  file <- getFileByPath path
  case file of
    Just doc@Document{} -> return doc
    Just Directory{} -> throwM DocumentExpected
    Nothing -> throwM NoSuchFile

findInPathByName :: File -> String -> [File]
findInPathByName root name = do
  let initial = (:[]) <$> findInFolder root name `orElse` []
  foldr foldFunc initial (filterDirectories $ directoryContents root)
  where
    foldFunc dir acc = acc ++ findInPathByName dir name

createFileByName :: Path -> String -> File -> Bool -> FileSystem File
createFileByName parentPath name file overwrite =
  createFile (parentPath <:| name) file overwrite

createFile :: Path -> File -> Bool -> FileSystem File
createFile path newFile overwrite = do
  absPath <- toAbsoluteFSPath path
  root <- gets rootDirectory
  newRoot <- createFileRecursively absPath root newFile overwrite
  updateFileSystemWithNewRoot newRoot
  created <- getFileByPath absPath
  case created of
    Just file -> return file
    Nothing -> throwM FailedToCreateFile

getAllFilesInSubDirectories :: File -> [File]
getAllFilesInSubDirectories doc@Document{} = [doc]
getAllFilesInSubDirectories Directory{ directoryContents = contents } =
  concatMap getAllFilesInSubDirectories contents

-- todo: check root
copyFile :: File -> Path -> FileSystem ()
copyFile file targetPath = do
  targetAbsPath <- toAbsoluteFSPath targetPath
  let newPath = targetAbsPath <:| (NE.last $ filePath file)
  root <- gets rootDirectory
  newRoot <- createFileRecursively newPath root file False
  updateFileSystemWithNewRoot newRoot
  case file of
    Directory{} -> updateFileSystemWithNewRoot $ updateParentsOfDirectoryContent newRoot
    Document{} -> return ()

updateParentsOfDirectoryContent :: File -> File
updateParentsOfDirectoryContent dir@Directory{ filePath = path, directoryContents = contents } =
  dir { directoryContents = Prelude.map (updateParents path) contents }
updateParentsOfDirectoryContent dir@Document{} = dir

updateParents :: Path -> File -> File
updateParents parentPath file@Document{}  =
  file
    { fileParent = Just parentPath
    , filePath = parentPath <:| (nameByPath . filePath $ file)
    }
updateParents parentPath file@Directory{ directoryContents = contents } = do
  let newPath = parentPath <:| (nameByPath . filePath $ file)
  file
    { fileParent = Just parentPath
    , filePath = newPath
    , directoryContents = Prelude.map (updateParents newPath) contents
    }

createFileRecursively :: Path -> File -> File -> Bool -> FileSystem File
createFileRecursively ("/" :| []) Directory{} _ _ = throwM CannotCreateRoot
createFileRecursively ("/" :| next) root@Directory{} newFile overwrite =
  createFileRecursively (NE.fromList next) root newFile overwrite
createFileRecursively (name :| []) root@Directory{ directoryContents = contents } newFile overwrite = do
  let fileWithUpdatedPaths = newFile {
      filePath = (filePath root) <:| name
    , fileParent = Just $ filePath root
    }
  let newRoot = root { directoryContents = contents ++ [fileWithUpdatedPaths] }
  case findInFolder root name of
    Just file -> 
      if (not overwrite) 
      then throwM FileAlreadyExists 
      else updateFileInDirectory newRoot file newFile
    Nothing -> return $ newRoot
createFileRecursively path@(name :| next) root@Directory{} newFile overwrite =
  case findInFolder root name of
    Just dir@(Directory {}) -> do
      new <- createFileRecursively (NE.fromList next) dir newFile overwrite
      updateFileInDirectory root dir new
    Nothing -> do
      let newDirectory = emptyDirectory {
          filePath = (filePath root) <:| name
        , fileParent = Just $ filePath root
        } 
      updatedRoot <- addToDirectory root newDirectory
      createFileRecursively path updatedRoot newFile overwrite
    Just (Document {}) -> throwM DirectoryExpected
createFileRecursively _ Document{} _ _ = throwM DirectoryExpected

removeFile :: Path -> FileSystem ()
removeFile path = do
   absPath <- toAbsoluteFSPath path
   currentDirPath <- filePath <$> gets currentDirectory
   if absPath `Path.isParentOf` currentDirPath
     then throwM CannotRemoveParent
     else gets rootDirectory >>= removeFileRecursively absPath >>= updateFileSystemWithNewRoot

removeFileRecursively :: Path -> File -> FileSystem File
removeFileRecursively ("/" :| []) Directory{} = throwM CannotRemoveRoot
removeFileRecursively ("/" :| next) root@Directory{}  =
  removeFileRecursively (NE.fromList next) root 
removeFileRecursively (name :| []) root@Directory{} =
  removeFromDirectory root <$> (findInFolder root name) `orElse` throwM FileNotFound
removeFileRecursively (name :| next) root@Directory{} =
  case findInFolder root name of
    Just dir@(Directory {}) -> do
      new <- removeFileRecursively (NE.fromList next) dir
      updateFileInDirectory root dir new
    Just Document{} -> throwM DirectoryExpected
    Nothing -> throwM FileNotFound
removeFileRecursively _ Document{} = throwM DirectoryExpected

updateFileSystemWithNewRoot :: File -> FileSystem ()
updateFileSystemWithNewRoot newRoot = do
  modify (\s -> s { rootDirectory = newRoot })
  newCurrentDir <- gets currentDirectory >>= getDirectoryByPath . filePath
  modify (\s -> s { currentDirectory = newCurrentDir })

addToDirectory :: File -> File -> FileSystem File
addToDirectory dir@Directory{ directoryContents = contents } file =
  return dir { directoryContents = contents ++ [file] }
addToDirectory _ _ = throwM DirectoryExpected

removeFromDirectory :: File -> File -> FileSystem File
removeFromDirectory dir@Directory{ directoryContents = contents } file =
  return dir { directoryContents = remove contents file }
removeFromDirectory _ _ = throwM DirectoryExpected

updateFileInDirectory :: File -> File -> File -> FileSystem File
updateFileInDirectory dir@Directory{ directoryContents = contents } file newFile =
  return dir { directoryContents = update contents file newFile }
updateFileInDirectory _ _ _ = throwM DirectoryExpected
