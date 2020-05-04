{-# LANGUAGE DeriveAnyClass #-}

module Filesystem where

import Control.Exception
import Control.Monad.State
import Data.List (find, intercalate)
import Data.List.NonEmpty as NE
import Utils

type Path = NonEmpty String

type AccessibilityRights = String

type FileSystem a = StateT FSState IO a

data File
  = Directory
      { filePath :: Path
      , fileAccessibility :: AccessibilityRights
      , directoryContents :: [File]
      , directoryParent :: Maybe File
      }
  | Document
      { filePath :: Path
      , fileAccessibility :: AccessibilityRights
      , documentExtension :: String
      , documentCreationTime :: String
      , documentUpdateTime :: String
      , documentSize :: Int
      }

instance Show File where
  show = pathToString . filePath

data FSState =
  FSState
    { currentDirectory :: File
    , rootDirectory :: File
    }

data CommandExecutionError
  = DirectoryIsDocument
  | NoSuchDirectory
  | FileAlreadyExists
  deriving (Show, Exception)


getDirectoryByPath :: String -> FileSystem File
getDirectoryByPath path = do
  currentDir <- gets currentDirectory
  rootDir <- gets rootDirectory
  let dir =
       case path of
         '/':_ -> rootDir
         _ -> currentDir
  return $ getDirectoryByRelativePath (stringToPath path) dir

getDirectoryByRelativePath :: Path -> File -> File
getDirectoryByRelativePath ("/" :| []) root =  root
getDirectoryByRelativePath ("/" :| path) root = getDirectoryByRelativePath (fromList path) root
getDirectoryByRelativePath (x :| []) root = moveNext root x
getDirectoryByRelativePath (x :| next:xs) root = getDirectoryByRelativePath (next :| xs) (moveNext root x)

moveNext :: File -> String -> File
moveNext root name =
  case findInFolder root name of
    Just dir@(Directory {}) -> dir
    Just (Document {}) -> throw DirectoryIsDocument
    Nothing -> throw NoSuchDirectory

findInFolder :: File -> String -> Maybe File
findInFolder root name =
  case name of
    "." -> Just root
    ".." ->
      case directoryParent root of
        Just p -> Just p
        Nothing -> Just root
    _ -> find ((== name) . nameByPath) (directoryContents root)

nameByPath :: File -> String
nameByPath = NE.last . filePath

stringToPath :: String -> Path
stringToPath = splitOn '/'

pathToString :: Path -> String
pathToString = intercalate "/" . NE.toList

constructDirectoryRelative :: File -> String -> File
constructDirectoryRelative parent name =
  Directory
    { filePath = filePath parent <:| name
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
