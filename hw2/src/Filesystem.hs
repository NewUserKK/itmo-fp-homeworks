{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Filesystem where

import Control.Exception
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.List (find, intercalate)
import Data.List.NonEmpty as NE
import Utils

type Path = NonEmpty String

type StringPath = String

type AccessibilityRights = String

type FileSystem a = StateT FSState IO a

data File
  = Directory
      { filePath :: Path
      , fileAccessibility :: AccessibilityRights
      , directoryContents :: [File]
      , directoryParent :: Maybe Path
      }
  | Document
      { filePath :: Path
      , fileAccessibility :: AccessibilityRights
      , documentExtension :: String
      , documentCreationTime :: String
      , documentUpdateTime :: String
      , documentSize :: Int
      , documentContent :: BS.ByteString
      }

instance Show File where
  show = pathToString . filePath

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
getFileByPath path = do
  currentDir <- gets currentDirectory
  rootDir <- gets rootDirectory
  let dir =
       case path of
         '/':_ -> rootDir
         _ -> currentDir
  getFileByRelativePath (stringToPath path) dir

getFileByRelativePath :: Path -> File -> FileSystem File
getFileByRelativePath _ Document{} = throw DirectoryExpected
getFileByRelativePath ("/" :| []) _ = gets rootDirectory
getFileByRelativePath ("/" :| path) _ = gets rootDirectory >>= getFileByRelativePath (fromList path)
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
    _ -> return $ find ((== name) . nameByPath) (directoryContents folder)

nameByPath :: File -> String
nameByPath = NE.last . filePath

stringToPath :: StringPath -> Path
stringToPath "/" = "/":|[]
stringToPath s = splitOn '/' s

pathToString :: Path -> StringPath
pathToString = intercalate "/" . NE.toList

constructDirectoryRelative :: Path -> String -> File
constructDirectoryRelative parent name =
  Directory
    { filePath = parent <:| name
    , fileAccessibility = ""
    , directoryContents = []
    , directoryParent = Just parent
    }

constructDirectoryByPath :: String -> File
constructDirectoryByPath path  =
  Directory
    { filePath = stringToPath path
    , fileAccessibility = ""
    , directoryContents = []
    , directoryParent = Nothing
    }
