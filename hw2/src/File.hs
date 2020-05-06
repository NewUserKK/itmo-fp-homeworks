{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module File where

import Data.List
import Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy as BS
import Data.Time (UTCTime)
import Path
import System.Directory
import GHC.Int (Int64)

data File
  = Directory
      { filePath :: Path
      , filePermissions :: Permissions
      , fileParent :: Maybe Path
      , directoryContents :: [File]
      }
  | Document
      { filePath :: Path
      , filePermissions :: Permissions
      , fileParent :: Maybe Path
      , documentCreationTime :: UTCTime
      , documentUpdateTime :: UTCTime
      , documentSize :: Int64
      , documentContent :: BS.ByteString
      }
  deriving (Eq)

instance Show File where
  show = pathToString . filePath

filterDirectories :: [File] -> [File]
filterDirectories = Prelude.filter (\case Directory{} -> True; _ -> False)

fileName :: File -> String
fileName = nameByPath . filePath

findInFolder :: File -> String -> Maybe File
findInFolder folder name = find ((name ==) . fileName) (directoryContents folder)

isParentOf :: File -> File -> Bool
isParentOf parent file = (filePath parent) `Path.isParentOf` (filePath file)

emptyDirectory :: Path -> File
emptyDirectory path = Directory {
   filePath = path
  , filePermissions = defaultPermissions
  , directoryContents = []
  , fileParent = Just $ getParentPath path
  }
  
emptyDocument :: Path -> UTCTime -> File
emptyDocument path creationTime = Document 
  { filePath = path
  , filePermissions = defaultPermissions
  , fileParent = Nothing
  , documentCreationTime = creationTime
  , documentUpdateTime = creationTime
  , documentSize = 0
  , documentContent = ""
  }
  
defaultPermissions :: Permissions
defaultPermissions = emptyPermissions {
  readable = True,
  writable = True,
  executable = False,
  searchable = True
}