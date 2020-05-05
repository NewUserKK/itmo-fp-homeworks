{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Filesystem where

import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import Data.List (find, intercalate)
import Data.List.NonEmpty as NE
import Utils
import Data.Time (UTCTime)
import System.Directory

type Path = NonEmpty String

type StringPath = String

type FileSystem a = StateT FSState IO a

data File
  = Directory
      { filePath :: Path
      , filePermissions :: Permissions
      , directoryContents :: [File]
      , directoryParent :: Maybe Path
      }
  | Document
      { filePath :: Path
      , filePermissions :: Permissions
      , documentExtension :: String
      , documentCreationTime :: UTCTime
      , documentUpdateTime :: UTCTime
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
    _ -> return $ find ((== name) . nameByPath) (directoryContents folder)

nameByPath :: File -> String
nameByPath = NE.last . filePath

stringToPath :: StringPath -> Path
stringToPath "/" = "/":|[]
stringToPath ('/':cs) = "/" :| NE.drop 1 (splitOn '/' cs)
stringToPath s = splitOn '/' s

pathToString :: Path -> StringPath
pathToString ("/":|[]) = "/" 
pathToString ("/":|cs) = "/" ++ (intercalate "/" cs) 
pathToString path = intercalate "/" . NE.toList $ path 

extensionFromPath :: Path -> String
extensionFromPath = NE.last . splitOn '.' . NE.last

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
        ".." -> safeInit acc
        s -> acc ++ [s]  

safeInit :: [String] -> [String]
safeInit [] = []
safeInit ["/"] = ["/"]
safeInit list = Prelude.init list