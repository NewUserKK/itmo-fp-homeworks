{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module CVS where

import Control.Monad.Catch (throwM)
import Control.Monad.State
import Data.Aeson
import Data.Algorithm.Diff (Diff, PolyDiff(..), getGroupedDiff)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Time (UTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import File
import Filesystem
import GHC.Generics
import Path

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

data MergeStrategy
  = MergeLeft
  | MergeRight
  | MergeBoth

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

cvsAdd :: Path -> FileSystem File
cvsAdd path = getFileByPathOrError path >>= cvsAddFile

cvsAddFile :: File -> FileSystem File
cvsAddFile file@Document{ filePath = path } = do
  createCVSRevisionDir path
  createNewRevision path 0 file "Initial revision"
cvsAddFile dir@Directory{} = do
  foldr ((>>) . cvsAddFile) (return ()) (getAllFilesInSubDirectories dir)
  return emptyDirectory

cvsUpdate :: Path -> String -> FileSystem File
cvsUpdate path comment = getFileByPathOrError path >>= flip cvsUpdateFile comment

cvsUpdateFile :: File -> String -> FileSystem File
cvsUpdateFile file@Document{ filePath = path } comment = do
  newRevIndex <- (+1) <$> getLatestRevisionIndex path
  createNewRevision path newRevIndex file comment
cvsUpdateFile Directory{} _ = throwM DocumentExpected

createNewRevision :: Path -> Int -> File -> String -> FileSystem File
createNewRevision filepath index file comment = do
  absPath <- toAbsoluteFSPath filepath
  cvsRevDir <- getRevisionDirOrError absPath
  newRevDir <- createFileByName (filePath cvsRevDir) (show index) emptyDirectory False
  time <- liftIO $ systemToUTCTime <$> getSystemTime
  _ <- createFileByName
    (filePath newRevDir)
    commitInfoFileName
    (constructCommitInfoFile absPath index comment time)
    True
  copyFile file (filePath newRevDir)
  getDirectoryByPath (filePath newRevDir)

createCVSRevisionDir :: Path -> FileSystem ()
createCVSRevisionDir path = do
  cvsDir <- getCVSForFileOrError path
  hash <- revisionHash path
  maybeRevDir <- getRevisionDir path
  case maybeRevDir of
    Nothing -> void $ createFileByName (filePath cvsDir) hash emptyDirectory False
    Just _ -> return ()

getCVSRevision :: Path -> Int -> FileSystem (Maybe File)
getCVSRevision path index = do
  cvsDir <- getRevisionDirOrError path
  return $ findInFolder cvsDir (show index)

getCVSRevisionOrError :: Path -> Int -> FileSystem File
getCVSRevisionOrError path index = do
  maybeRevision <- getCVSRevision path index
  case maybeRevision of
    Just revision -> return revision
    Nothing -> throwM UnknownRevision

removeFromCVS :: Path -> FileSystem ()
removeFromCVS path = do
  cvsDir <- getRevisionDirOrError path
  removeFile $ filePath cvsDir

removeRevision :: Path -> Int -> FileSystem ()
removeRevision path index = do
  revision <- getCVSRevisionOrError path index
  removeFile $ filePath revision

getRevisionDir :: Path -> FileSystem (Maybe File)
getRevisionDir path = do
  cvsDir <- getCVSForFileOrError path
  hash <- revisionHash path
  return $ findInFolder cvsDir hash

getRevisionDirOrError :: Path -> FileSystem File
getRevisionDirOrError path = do
  revDir <- getRevisionDir path
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
  revDir <- getRevisionDirOrError path
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
  getRevisionDirOrError (filePath path) >>=
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

mergeRevisions :: File -> File -> MergeStrategy -> FileSystem File
mergeRevisions revision1 revision2 strategy = do
  content1 <- BS.lines . documentContent <$> getFileFromRevisionDir revision1
  content2 <- BS.lines . documentContent <$> getFileFromRevisionDir revision2
  let diff = getGroupedDiff content1 content2
  let newLines =
        case strategy of
          MergeLeft -> content1
          MergeRight -> content2
          MergeBoth -> mergeBoth diff
  let newContent = BS.intercalate "\n" newLines

  path <- getCommitInfo revision2 >>= return . commitRealFilePath
  revIndex1 <- getCommitInfo revision1 >>= return . commitIndex
  revIndex2 <- getCommitInfo revision2 >>= return . commitIndex
  let comment = "Merge revisions " ++ show revIndex1 ++ " and " ++ show revIndex2
  newRevDir <- cvsUpdate path comment
  newFile <- getFileFromRevisionDir newRevDir
  let newPath = filePath newFile
  _ <- createFile newPath newFile{ documentContent = newContent } True
  let fsFile = newFile {
      filePath = path
    , fileParent = Just $ getParentPath path
    , documentContent = newContent
  }
  _ <- createFile path fsFile True

  getDirectoryByPath $ filePath newRevDir

mergeBoth :: [Diff [BS.ByteString]] -> [BS.ByteString]
mergeBoth diff = concatMap mergeFunc diff
  where
    mergeFunc (First a) = a
    mergeFunc (Both a _) = a
    mergeFunc (Second b) = b

filterAddedToCvs :: [File] -> FileSystem [File]
filterAddedToCvs list = catMaybes <$> traverse mapFunc list
  where
    mapFunc file = do
      dir <- getRevisionDir $ filePath file
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
