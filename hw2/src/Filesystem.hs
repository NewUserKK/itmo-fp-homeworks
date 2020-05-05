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
  deriving (Show, Exception)


getFileByPath :: StringPath -> FileSystem File
getFileByPath stringPath = do
  absolutePath <- toAbsolutePath stringPath
  liftIO $ print absolutePath
  getFileByAbsolutePath absolutePath

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

getDirectoryByPath :: StringPath -> FileSystem File
getDirectoryByPath path = do
  file <- getFileByPath path
  case file of
    Directory{} -> return file
    Document{} -> throw DirectoryExpected

getDocumentByPath :: StringPath -> FileSystem File
getDocumentByPath path = do
  file <- getFileByPath path
  case file of
    Document{} -> return file
    Directory{} -> throw DocumentExpected

moveNext :: File -> String -> FileSystem File
moveNext root name = do
  file <- findInFolder root name
  case file of
    Just f -> return f
    Nothing -> throw NoSuchDirectory

findInFolder :: File -> String -> FileSystem (Maybe File)
findInFolder folder name = do
  case name of
    "." -> return $ Just folder
    ".." ->
      case directoryParent folder of
        Just path -> Just <$> getFileByPath (pathToString path)
        Nothing -> return $ Just folder
    _ -> return $ find ((name ==) . fileName) (directoryContents folder)

constructDirectoryRelative :: Path -> String -> File
constructDirectoryRelative parent name =
  Directory
    { filePath = parent <:| name
    , filePermissions = emptyPermissions
    , directoryContents = []
    , directoryParent = Just parent
    }

constructDirectoryByPath :: String -> File
constructDirectoryByPath path  =
  Directory
    { filePath = stringToPath path
    , filePermissions = emptyPermissions
    , directoryContents = []
    , directoryParent = Nothing
    }

toAbsolutePath :: StringPath -> FileSystem Path
toAbsolutePath path = do
  fsRoot <- gets rootDirectory
  currentDir <- gets currentDirectory
  return $ case path of
    '/':cs -> _toAbsolutePath (stringToPath cs) fsRoot
    _ -> _toAbsolutePath (stringToPath path) currentDir

_toAbsolutePath :: Path -> File -> Path
_toAbsolutePath path root = NE.fromList $ foldl foldFunc (NE.toList $ filePath root) path
  where
    foldFunc acc dir =
      case dir of
        "." -> acc
        ".." -> toParentPath acc
        s -> acc ++ [s]

toParentPath :: [String] -> [String]
toParentPath [] = []
toParentPath ["/"] = ["/"]
toParentPath list = Prelude.init list
