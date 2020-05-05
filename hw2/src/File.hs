{-# LANGUAGE LambdaCase #-}

module File where

import Data.ByteString.Lazy as BS
import Data.Time (UTCTime)
import Path
import System.Directory (Permissions)

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
