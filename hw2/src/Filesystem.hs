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
  | NoSuchDirectory
  | FileAlreadyExists
  | CannotCreateRoot
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
    Nothing -> throwM NoSuchDirectory

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

createFile :: StringPath -> File -> FileSystem ()
createFile stringPath newFile = do
   path <- toAbsolutePath stringPath
   root <- gets rootDirectory
   newRoot <- createFileRecursively path root newFile
   modify (\s -> s { rootDirectory = newRoot })
   newCurrentDir <- gets currentDirectory >>= getFileByPath . pathToString . filePath
   modify (\s -> s { currentDirectory = newCurrentDir })

createFileRecursively :: Path -> File -> File -> FileSystem File
createFileRecursively ("/" :| []) Directory{} _ = throwM CannotCreateRoot
createFileRecursively ("/" :| next) root@Directory{} newFile =
  createFileRecursively (NE.fromList next) root newFile
createFileRecursively (name :| []) root@Directory{ directoryContents = contents } newFile =
  case findInFolder root name of
    Just _ -> throwM FileAlreadyExists
    Nothing -> do
      let fileWithUpdatedPaths = newFile {
          filePath = (filePath root) <:| name
        , fileParent = Just $ filePath root
        }
      return $ root { directoryContents = contents ++ [fileWithUpdatedPaths] }
createFileRecursively path@(name :| next) root@Directory{ directoryContents = contents } newFile =
  case findInFolder root name of
    Just dir@(Directory {}) -> do
      new <- createFileRecursively (NE.fromList next) dir newFile
      return $ root {
        directoryContents = Prelude.filter ((/=) dir) contents ++ [new]
      }
    Nothing -> do
      let newDirectory = Directory {
          filePath = (filePath root) <:| name
        , filePermissions = emptyPermissions
        , fileParent = Just $ filePath root
        , directoryContents = []
        }
      createFileRecursively path (root {
        directoryContents = directoryContents root ++ [newDirectory]
      }) newFile
    Just (Document {}) -> throwM DirectoryExpected
createFileRecursively _ Document{} _ = throwM DirectoryExpected
