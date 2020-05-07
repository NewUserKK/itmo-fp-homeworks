{-# LANGUAGE DeriveGeneric #-}

module CVS where

import Control.Monad.Catch (throwM)
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust, mapMaybe)
import Data.Time (UTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import File
import Filesystem
import GHC.Generics
import Path
import Utils (readMaybeInt)

data CommitInfo =
  CommitInfo
    { commitIndex :: Int
    , commitPath :: Path
    , commitMessage :: String
    }
  deriving (Generic)

instance FromJSON CommitInfo
instance ToJSON CommitInfo

instance Show CommitInfo where
  show info = (show $ commitIndex info) ++ ". " ++ (commitMessage info)

commitInfoFileName :: String
commitInfoFileName = "COMMIT_INFO"

cvsFileName :: String
cvsFileName = ".cvs"


cvsInit :: Path -> FileSystem File
cvsInit path = do
  existing <- getCvsForFile path
  case existing of
    Just cvs -> throwM $ CVSAlreadyExists (pathToString $ filePath cvs)
    Nothing -> createFileByName path cvsFileName emptyDirectory False

cvsAdd :: Path -> FileSystem ()
cvsAdd path = getFileByPathOrError path >>= cvsAddFile

cvsAddFile :: File -> FileSystem ()
cvsAddFile file@Document{ filePath = path } = do
  createCVSRevisionDir path
  createNewRevision path 0 file "Initial revision"
cvsAddFile Directory{ directoryContents = contents } =
  foldr ((>>) . cvsAddFile) (return ()) contents

cvsUpdate :: Path -> String -> FileSystem ()
cvsUpdate path comment = getFileByPathOrError path >>= cvsUpdateFile comment
  
cvsUpdateFile :: String -> File -> FileSystem ()
cvsUpdateFile comment file@Document{ filePath = path } = do
  newRevIndex <- (+1) <$> getLatestRevisionIndex path
  createNewRevision path newRevIndex file comment
cvsUpdateFile _ Directory{} = throwM DocumentExpected

createNewRevision :: Path -> Int -> File -> String -> FileSystem ()
createNewRevision filepath index file comment = do
  absPath <- toAbsoluteFSPath filepath
  cvsRevDir <- getCVSRevisionDirOrError absPath
  newRevDir <- createFileByName (filePath cvsRevDir) (show index) emptyDirectory False
  time <- liftIO $ systemToUTCTime <$> getSystemTime
  _ <- createFileByName
    (filePath newRevDir)
    commitInfoFileName
    (constructCommitInfoFile absPath index comment time)
    True
  copyFile file (filePath newRevDir)

createCVSRevisionDir :: Path -> FileSystem ()
createCVSRevisionDir path = do
  cvsDir <- getCvsForFileOrError path
  hash <- revisionHash path
  maybeRevDir <- getCVSRevisionDir path
  case maybeRevDir of
    Nothing -> void $ createFileByName (filePath cvsDir) hash emptyDirectory False
    Just _ -> return ()

getCVSRevision :: Path -> Int -> FileSystem (Maybe File)
getCVSRevision path index = do
  cvsDir <- getCVSRevisionDirOrError path
  return $ findInFolder cvsDir (show index)

getCVSRevisionDir :: Path -> FileSystem (Maybe File)
getCVSRevisionDir path = do
  cvsDir <- getCvsForFileOrError path
  hash <- revisionHash path
  return $ findInFolder cvsDir hash

getCVSRevisionDirOrError :: Path -> FileSystem File
getCVSRevisionDirOrError path = do
  revDir <- getCVSRevisionDir path
  case revDir of
    Just dir@Directory{} -> return dir
    Just Document{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM FileNotAddedToCVS

getCvsForFile :: Path -> FileSystem (Maybe File)
getCvsForFile path = do
  file <- getFileByPath path
  dir <- case file of
    Just Document{ fileParent = parent } -> getDirectoryByPath (fromJust parent)
    Just d@Directory{} -> return d
    Nothing -> throwM NoSuchFile
  case findInFolder dir cvsFileName of
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

getLatestRevisionIndex :: Path -> FileSystem Int
getLatestRevisionIndex path = do
  revDir <- getCVSRevisionDirOrError path
  return $ maximum $ mapMaybe (readMaybeInt . fileName) (directoryContents revDir)

getAllRevisionsOfFile :: Path -> FileSystem [File]
getAllRevisionsOfFile path = getCVSRevisionDirOrError path >>= return . directoryContents

getCommitInfoFromRevisionDir :: File -> FileSystem CommitInfo
getCommitInfoFromRevisionDir dir = do
  case findInFolder dir commitInfoFileName of
    Just info@Document{} -> deserializeCommitInfo (documentContent info)
    Just Directory{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM InvalidCVSRevisionDirectory

constructCommitInfoFile :: Path -> Int -> String -> UTCTime -> File
constructCommitInfoFile committedFilePath index comment creationTime = do
  let commitInfo = CommitInfo {
      commitIndex = index
    , commitPath = committedFilePath
    , commitMessage = comment
    }
  (emptyDocument creationTime)
    { documentContent = serializeCommitInfo commitInfo
    }

revisionHash :: Path -> FileSystem String
revisionHash path = toAbsoluteFSPath path >>= return . pathHash

deserializeCommitInfo :: BS.ByteString -> FileSystem CommitInfo
deserializeCommitInfo content = 
  case eitherDecode content of
    Right info -> return info
    Left _ -> throwM MalformedCommitInfo

serializeCommitInfo :: CommitInfo -> BS.ByteString
serializeCommitInfo = encode
