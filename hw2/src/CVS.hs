module CVS where

import Path
import File
import Filesystem as FS
import Control.Monad.State
import Control.Monad.Catch (throwM)
import Data.Maybe (fromJust)

cvsInit :: Path -> FileSystem File
cvsInit path = do
  existing <- getCvsForFile path
  case existing of
    Just cvs -> throwM $ CvsExists (pathToString $ filePath cvs)
    Nothing -> FS.createFileByName path ".cvs" (emptyDirectory path) False

cvsAdd :: Path -> FileSystem ()
cvsAdd path = do
  maybeFile <- getFileByPath path
  file <- case maybeFile of
    Just f -> return f
    Nothing -> throwM NoSuchFile
  maybeCvsDir <- getCvsForFile path
  hash <- revisionHash path
  case maybeCvsDir of
    Nothing -> throwM $ CvsDoesNotExist
    Just cvsDir -> 
      case findInFolder cvsDir hash of
        Just _ -> return ()
        Nothing -> do
          fileDir <- createFileByName (filePath cvsDir) hash (emptyDirectory emptyPath) False
          revDir <- createFileByName (filePath fileDir) "0" (emptyDirectory emptyPath) False
          copyFile file (filePath revDir)
          
getCvsForFile :: Path -> FileSystem (Maybe File)
getCvsForFile path = do
  file <- getFileByPath path
  dir <- case file of
    Just Document{ fileParent = parent } -> getDirectoryByPath (fromJust parent)
    Just d@Directory{} -> return d
    Nothing -> throwM NoSuchFile
  case findInFolder dir ".cvs" of
    Just cvs@Directory{} -> return $ Just cvs
    Just Document{} -> throwM InvalidCvsFolder
    Nothing ->
      case (fileParent dir) of
        Just parent -> getCvsForFile parent
        Nothing -> return Nothing
        
revisionHash :: Path -> FileSystem String
revisionHash path = toAbsoluteFSPath path >>= return . pathHash 