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
moveNext root name = 
  case findInFolder root name of
    Just f -> return f
    Nothing -> throw NoSuchDirectory


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