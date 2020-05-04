{-# LANGUAGE DeriveAnyClass #-}

module Filesystem where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Reader
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

changeDirectory :: String -> FileSystem ()
changeDirectory path = do
  st <- get
  evalled <- liftIO $ evalStateT (getDirectoryByPath path) st
  case evalled of
    Left e -> throwM e
    Right newDirectory -> do
      liftIO $ print . directoryParent $ newDirectory
      modify (\s -> s {currentDirectory = newDirectory})

listContents :: String -> FileSystem (Either CommandExecutionError [String])
listContents path = do
  dir <- getDirectoryByPath path
  return $ dir >>= return . (Prelude.map nameByPath) . directoryContents

makeDirectory :: String -> FileSystem ()
makeDirectory path = do
  root <- gets rootDirectory
  let e = mkdir (stringToPath path) root
  case e of
    Left t -> throwM t
    Right new -> modify (\s -> s {rootDirectory = new})

mkdir :: Path -> File -> Either CommandExecutionError File
mkdir path@(x :| next : xs) root =
  case findInFolder root x of
    Just dir@(Directory {}) -> mkdir (next :| xs) dir
    Just (Document {}) -> Left DirectoryIsDocument
    Nothing -> mkdir path (root {
      directoryContents = directoryContents root ++ [constructDirectoryRelative root x]
    })
mkdir (x :| []) root =
  case findInFolder root x of
    Just _ -> Left FileAlreadyExists
    Nothing -> Right $ (root {
      directoryContents = directoryContents root ++ [constructDirectoryRelative root x]
    })

getDirectoryByPath :: String -> FileSystem (Either CommandExecutionError File)
getDirectoryByPath path = do
  currentDir <- gets currentDirectory
  rootDir <- gets rootDirectory
  let dir =
       case path of
         '/':_ -> rootDir
         _ -> currentDir
  return $ getDirectoryByRelativePath (stringToPath path) dir

getDirectoryByRelativePath :: Path -> File -> Either CommandExecutionError File
getDirectoryByRelativePath ("/" :| []) root = Right root
getDirectoryByRelativePath ("/" :| path) root = getDirectoryByRelativePath (fromList path) root
getDirectoryByRelativePath (x :| []) root = moveNext root x
getDirectoryByRelativePath (x :| next:xs) root = moveNext root x >>= getDirectoryByRelativePath (next :| xs)

moveNext :: File -> String -> Either CommandExecutionError File
moveNext root name =
  case findInFolder root name of
    Just dir@(Directory {}) -> Right dir
    Just (Document {}) -> Left DirectoryIsDocument
    Nothing -> Left NoSuchDirectory

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
