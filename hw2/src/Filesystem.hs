{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Filesystem where

import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import Data.List (find, intercalate)
import Data.List.NonEmpty as NE
import Utils
import Path
import File
import Data.Time (UTCTime)
import System.Directory
import Control.Applicative ((<|>))
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
  | CannotCreateRoot
  | CannotRemoveRoot
  deriving (Show, Exception)


toAbsolutePath :: StringPath -> FileSystem Path
toAbsolutePath stringPath = do
  fsRoot <- gets rootDirectory
  currentDir <- gets currentDirectory
  return $ case stringPath of
    "/" -> stringToPath "/"
    '/':cs -> _toAbsolutePath (stringToPath cs) fsRoot
    _ -> _toAbsolutePath (stringToPath stringPath) currentDir
  where
    _toAbsolutePath :: Path -> File -> Path
    _toAbsolutePath path root = foldl foldFunc (filePath root) path
      where
        foldFunc acc dir =
          case dir of
            "." -> acc
            ".." -> getParentPath acc
            s -> acc <:| s

getFileByPath :: StringPath -> FileSystem File
getFileByPath stringPath = toAbsolutePath stringPath >>= getFileByAbsolutePath

getFileByAbsolutePath :: Path -> FileSystem File
getFileByAbsolutePath path = do
  root <- gets rootDirectory
  case path of
    ("/" :| []) -> return root
    ("/" :| remaining) -> getFileByRelativePath (NE.fromList remaining) root
    relativePath -> getFileByRelativePath relativePath root

getFileByRelativePath :: Path -> File -> FileSystem File
getFileByRelativePath _ Document{} = throw DirectoryExpected
getFileByRelativePath absPath@("/" :| _) _ = getFileByAbsolutePath absPath
getFileByRelativePath (x :| []) root = moveNext root x
getFileByRelativePath (x :| next : xs) root = moveNext root x >>= getFileByRelativePath (next :| xs)

moveNext :: File -> String -> FileSystem File
moveNext root name =
  case findInFolder root name of
    Just f -> return f
    Nothing -> throwM NoSuchFile

getDirectoryByPath :: StringPath -> FileSystem File
getDirectoryByPath path = do
  file <- getFileByPath path
  case file of
    Directory{} -> return file
    Document{} -> throwM DirectoryExpected

getDocumentByPath :: StringPath -> FileSystem File
getDocumentByPath path = do
  file <- getFileByPath path
  case file of
    Document{} -> return file
    Directory{} -> throwM DocumentExpected

findInPathByName :: File -> String -> [File]
findInPathByName root name = do
  let initial =
        case findInFolder root name of
          Just file -> [file]
          Nothing -> []
  foldr foldFunc initial (filterDirectories $ directoryContents root)
  where
    foldFunc dir acc =
      acc ++ findInPathByName dir name

createFileOverwriting :: StringPath -> File -> FileSystem ()
createFileOverwriting stringPath newFile = createFile stringPath newFile True

createFile :: StringPath -> File -> Bool -> FileSystem ()
createFile stringPath newFile overwrite = do
   path <- toAbsolutePath stringPath
   root <- gets rootDirectory
   newRoot <- createFileRecursively path root newFile overwrite
   updateFileSystemWithNewRoot newRoot

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
      let newDirectory = Directory {
          filePath = (filePath root) <:| name
        , filePermissions = emptyPermissions
        , fileParent = Just $ filePath root
        , directoryContents = []
        }
      updatedRoot <- addToDirectory root newDirectory
      createFileRecursively path updatedRoot newFile overwrite
    Just (Document {}) -> throwM DirectoryExpected
createFileRecursively _ Document{} _ _ = throwM DirectoryExpected

removeFile :: StringPath -> FileSystem ()
removeFile stringPath = do
   path <- toAbsolutePath stringPath
   gets rootDirectory >>= removeFileRecursively path >>= updateFileSystemWithNewRoot

removeFileRecursively :: Path -> File -> FileSystem File
removeFileRecursively ("/" :| []) Directory{} = throwM CannotRemoveRoot
removeFileRecursively ("/" :| next) root@Directory{}  =
  removeFileRecursively (NE.fromList next) root 
removeFileRecursively (name :| []) root@Directory{} = do
  case findInFolder root name of
    Just file -> removeFromDirectory root file
    Nothing -> throwM FileNotFound
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
  newCurrentDir <- gets currentDirectory >>= getFileByPath . pathToString . filePath
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
