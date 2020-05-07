{-# LANGUAGE DeriveGeneric #-}

module CVS where

import Control.Monad.Catch (throwM)
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust, mapMaybe, catMaybes)
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
    , commitRealFilePath :: Path
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
  existing <- getCVSForFile path
  case existing of
    Just cvs -> throwM $ CVSAlreadyExists (pathToString $ filePath cvs)
    Nothing -> createFileByName path cvsFileName emptyDirectory False

cvsAdd :: Path -> FileSystem ()
cvsAdd path = getFileByPathOrError path >>= cvsAddFile

cvsAddFile :: File -> FileSystem ()
cvsAddFile file@Document{ filePath = path } = do
  createCVSRevisionDir path
  createNewRevision path 0 file "Initial revision"
cvsAddFile dir@Directory{} =
  foldr ((>>) . cvsAddFile) (return ()) (getAllFilesInSubDirectories dir)

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
  cvsDir <- getCVSForFileOrError path
  hash <- revisionHash path
  maybeRevDir <- getCVSRevisionDir path
  case maybeRevDir of
    Nothing -> void $ createFileByName (filePath cvsDir) hash emptyDirectory False
    Just _ -> return ()

getCVSRevision :: Path -> Int -> FileSystem (Maybe File)
getCVSRevision path index = do
  cvsDir <- getCVSRevisionDirOrError path
  return $ findInFolder cvsDir (show index)

getCVSRevisionOrError :: Path -> Int -> FileSystem File
getCVSRevisionOrError path index = do
  maybeRevision <- getCVSRevision path index
  case maybeRevision of
    Just revision -> return revision
    Nothing -> throwM UnknownRevision

removeFromCVS :: Path -> FileSystem ()
removeFromCVS path = do
  cvsDir <- getCVSRevisionDirOrError path
  removeFile $ filePath cvsDir

removeRevision :: Path -> Int -> FileSystem ()
removeRevision path index = do
  revision <- getCVSRevisionOrError path index
  removeFile $ filePath revision

getCVSRevisionDir :: Path -> FileSystem (Maybe File)
getCVSRevisionDir path = do
  cvsDir <- getCVSForFileOrError path
  hash <- revisionHash path
  return $ findInFolder cvsDir hash

getCVSRevisionDirOrError :: Path -> FileSystem File
getCVSRevisionDirOrError path = do
  revDir <- getCVSRevisionDir path
  case revDir of
    Just dir@Directory{} -> return dir
    Just Document{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM FileNotAddedToCVS

getCVSForFile :: Path -> FileSystem (Maybe File)
getCVSForFile path = do
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
        Just parent -> getCVSForFile parent
        Nothing -> return Nothing

getCVSForFileOrError :: Path -> FileSystem File
getCVSForFileOrError path = do
  maybeCvs <- getCVSForFile path
  case maybeCvs of
    Just cvs@Directory{} -> return cvs
    Just Document{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM CVSDoesNotExist

getLatestRevisionIndex :: Path -> FileSystem Int
getLatestRevisionIndex path = do
  revDir <- getCVSRevisionDirOrError path
  let revisions = mapMaybe (readMaybeInt . fileName) (directoryContents revDir)
  if (null revisions)
    then return (-1)
    else return $ maximum revisions

getAllRevisionsOfDirectory :: File -> FileSystem [[File]]
getAllRevisionsOfDirectory dir@Directory{} = do
  addedToCvs <- filterAddedToCvs $ getAllFilesInSubDirectories dir
  liftIO $ print addedToCvs
  traverse getAllRevisionsOfDocument addedToCvs
getAllRevisionsOfDirectory Document{} = throwM DirectoryExpected

getAllRevisionsOfDocument :: File -> FileSystem [File]
getAllRevisionsOfDocument path =
  getCVSRevisionDirOrError (filePath path) >>=
  return . directoryContents

getCommitInfo :: File -> FileSystem CommitInfo
getCommitInfo revisionDir = do
  case findInFolder revisionDir commitInfoFileName of
    Just info@Document{} -> deserializeCommitInfo (documentContent info)
    Just Directory{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM InvalidCVSRevisionDirectory

getFileFromRevisionDir :: File -> FileSystem File
getFileFromRevisionDir revisionDir = do
  realPath <- commitRealFilePath <$> getCommitInfo revisionDir
  let name = nameByPath realPath
  case findInFolder revisionDir name of
    Just doc@Document{} -> return doc
    Just Directory{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM InvalidCVSRevisionDirectory

constructCommitInfoFile :: Path -> Int -> String -> UTCTime -> File
constructCommitInfoFile committedFilePath index comment creationTime = do
  let commitInfo = CommitInfo {
      commitIndex = index
    , commitRealFilePath = committedFilePath
    , commitMessage = comment
    }
  (emptyDocument creationTime)
    { documentContent = serializeCommitInfo commitInfo
    }

filterAddedToCvs :: [File] -> FileSystem [File]
filterAddedToCvs list = catMaybes <$> traverse mapFunc list
  where
    mapFunc file = do
      dir <- getCVSRevisionDir $ filePath file
      return $ (file <$ dir)

revisionHash :: Path -> FileSystem String
revisionHash path = toAbsoluteFSPath path >>= return . pathHash

deserializeCommitInfo :: BS.ByteString -> FileSystem CommitInfo
deserializeCommitInfo content = 
  case eitherDecode content of
    Right info -> return info
    Left _ -> throwM MalformedCommitInfo

serializeCommitInfo :: CommitInfo -> BS.ByteString
serializeCommitInfo = encode
