module CVS where

import Path
import File
import Filesystem as FS
import Control.Monad.State
import Control.Monad.Catch (throwM)
import Data.Maybe (fromJust, mapMaybe)
import Data.List (sort)
import Utils (readMaybeInt)
import Data.Time (UTCTime)
import Data.Time.Clock.System (systemToUTCTime, getSystemTime)

cvsInit :: Path -> FileSystem File
cvsInit path = do
  existing <- getCvsForFile path
  case existing of
    Just cvs -> throwM $ CVSAlreadyExists (pathToString $ filePath cvs)
    Nothing -> FS.createFileByName path ".cvs" emptyDirectory False

cvsAdd :: Path -> FileSystem ()
cvsAdd path = do
  file <- getFileByPathOrError path
  createCVSRevisionDir path
  createNewRevision path 0 file "Initial revision"

cvsUpdate :: Path -> String -> FileSystem ()
cvsUpdate path comment = do
  cvsRevDir <- getCVSRevisionDirOrError path
  file <- getFileByPathOrError path
  let newRev = (+1) $ maximum $ mapMaybe (readMaybeInt . fileName) (directoryContents cvsRevDir)
  createNewRevision path newRev file comment

createNewRevision :: Path -> Int -> File -> String -> FileSystem ()
createNewRevision filepath index file comment = do
  absPath <- toAbsoluteFSPath filepath
  cvsRevDir <- getCVSRevisionDirOrError absPath
  newRevDir <- createFileByName (filePath cvsRevDir) (show index) emptyDirectory False
  time <- liftIO $ systemToUTCTime <$> getSystemTime
  _ <- createFileByName
    (filePath newRevDir)
    "COMMIT_INFO"
    (constructCommitInfoFile absPath comment time)
    True
  copyFile file (filePath newRevDir)

getCVSRevisionDir :: Path -> FileSystem (Maybe File)
getCVSRevisionDir path = do
  cvsDir <- getCvsForFileOrError path
  hash <- revisionHash path
  return $ findInFolder cvsDir hash
    
getCVSRevisionDirOrError :: Path -> FileSystem File
getCVSRevisionDirOrError path = do
  revDir <- getCVSRevisionDir path
  case revDir of
    Just Directory{ directoryContents = [] } -> throwM InvalidCVSRevisionDirectory
    Just dir@Directory{} -> return dir
    Just Document{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM FileNotAddedToCVS
    
createCVSRevisionDir :: Path -> FileSystem ()
createCVSRevisionDir path = do
  cvsDir <- getCvsForFileOrError path
  hash <- revisionHash path
  maybeRevDir <- getCVSRevisionDir path
  case maybeRevDir of
    Nothing -> void $ createFileByName (filePath cvsDir) hash emptyDirectory False
    Just _ -> return ()
    
getCvsForFile :: Path -> FileSystem (Maybe File)
getCvsForFile path = do
  file <- getFileByPath path
  dir <- case file of
    Just Document{ fileParent = parent } -> getDirectoryByPath (fromJust parent)
    Just d@Directory{} -> return d
    Nothing -> throwM NoSuchFile
  case findInFolder dir ".cvs" of
    Just cvs@Directory{} -> return $ Just cvs
    Just Document{} -> throwM InvalidCVSRepository
    Nothing ->
      case (fileParent dir) of
        Just parent -> getCvsForFile parent
        Nothing -> return Nothing

getCvsForFileOrError :: Path -> FileSystem File
getCvsForFileOrError path = do
  maybeCvs <- getCvsForFile path
  case maybeCvs of
    Just cvs@Directory{} -> return cvs
    Just Document{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM CVSDoesNotExist

constructCommitInfoFile :: Path -> String -> UTCTime -> File
constructCommitInfoFile commitedFilePath comment creationTime = 
  (emptyDocument creationTime)
    { documentContent = (pathToString commitedFilePath) ++ "\n" ++ comment
    }

revisionHash :: Path -> FileSystem String
revisionHash path = toAbsoluteFSPath path >>= return . pathHash
