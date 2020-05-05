{-# LANGUAGE LambdaCase #-}

module File where

import Data.List
import qualified Data.ByteString.Lazy as BS
import Data.Time (UTCTime)
import Path
import System.Directory (Permissions, emptyPermissions)
import Utils ((<:|))

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

filterDirectories :: [File] -> [File]
filterDirectories = Prelude.filter (\case Directory{} -> True; _ -> False)

fileName :: File -> String
fileName = nameByPath . filePath

findInFolder :: File -> String -> Maybe File
findInFolder folder name = find ((name ==) . fileName) (directoryContents folder)

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
