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

createFileOverwriting :: StringPath -> File -> FileSystem ()
createFileOverwriting stringPath newFile = createFile stringPath newFile True

createFile :: StringPath -> File -> Bool -> FileSystem ()
createFile stringPath newFile overwrite = do
   path <- toAbsolutePath stringPath
   root <- gets rootDirectory
   newRoot <- createFileRecursively path root newFile overwrite
   modify (\s -> s { rootDirectory = newRoot })
   newCurrentDir <- gets currentDirectory >>= getFileByPath . pathToString . filePath
   modify (\s -> s { currentDirectory = newCurrentDir })

createFileRecursively :: Path -> File -> File -> Bool -> FileSystem File
createFileRecursively ("/" :| []) Directory{} _ _ = throwM CannotCreateRoot
createFileRecursively ("/" :| next) root@Directory{} newFile overwrite =
  createFileRecursively (NE.fromList next) root newFile overwrite
createFileRecursively (name :| []) root@Directory{ directoryContents = contents } newFile overwrite = do
  let fileWithUpdatedPaths = newFile {
      filePath = (filePath root) <:| name
    , fileParent = Just $ filePath root
    }
  let newRoot = root { directoryContents = contents ++ [fileWithUpdatedPaths] }
  case findInFolder root name of
    Just file -> 
      if (not overwrite) 
      then throwM FileAlreadyExists 
      else return $ updateFileInDirectory newRoot file newFile 
    Nothing -> return $ newRoot
createFileRecursively path@(name :| next) root@Directory{} newFile overwrite =
  case findInFolder root name of
    Just dir@(Directory {}) -> do
      new <- createFileRecursively (NE.fromList next) dir newFile overwrite
      return $ updateFileInDirectory root dir new
    Nothing -> do
      let newDirectory = Directory {
          filePath = (filePath root) <:| name
        , filePermissions = emptyPermissions
        , fileParent = Just $ filePath root
        , directoryContents = []
        }
      createFileRecursively path (addToDirectory root newDirectory) newFile overwrite
    Just (Document {}) -> throwM DirectoryExpected
createFileRecursively _ Document{} _ _ = throwM DirectoryExpected

addToDirectory :: File -> File -> File
addToDirectory dir@Directory{ directoryContents = contents } file = 
  dir { directoryContents = contents ++ [file] } 
addToDirectory _ _ = throw DirectoryExpected

removeFromDirectory :: File -> File -> File
removeFromDirectory dir@Directory{ directoryContents = contents } file = 
  dir { directoryContents = remove contents file } 
removeFromDirectory _ _ = throw DirectoryExpected

updateFileInDirectory :: File -> File -> File -> File
updateFileInDirectory dir@Directory{ directoryContents = contents } file newFile =
  dir { directoryContents = update contents file newFile }
updateFileInDirectory _ _ _ = throw DirectoryExpected

remove :: Eq a => [a] -> a -> [a]
remove contents file = Prelude.filter ((/=) file) contents

update :: Eq a => [a] -> a -> a -> [a]
update contents file newFile = remove contents file ++ [newFile]