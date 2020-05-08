{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module CVSCore
  ( CommitInfo(..)
  , MergeStrategy(..)
  , cvsInit
  , cvsAdd
  , cvsUpdate
  , getRevision
  , getRevisionOrError
  , removeFromCVS
  , removeRevision
  , getAllRevisionsOfDirectory
  , getAllRevisionsOfDocument
  , getCommitInfo
  , getFileFromRevision
  , mergeRevisions
  ) where

import Control.Monad.Catch (throwM)
import Control.Monad.State
import Data.Aeson
import Data.Algorithm.Diff (Diff, PolyDiff(..), getGroupedDiff)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (catMaybes, mapMaybe)
import Data.Time (UTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import File
import FilesystemCore
import GHC.Generics
import Path
import Utils
import Data.List.NonEmpty (NonEmpty(..))

{-|
  Structure representing commit info.
-}
data CommitInfo =
  CommitInfo
    { commitIndex :: Int          -- ^ index of revision
    , commitRealFilePath :: Path  -- ^ path to the file which revision was created
    , commitMessage :: String     -- ^ revision commit message
    }
  deriving (Generic, Eq)

instance FromJSON CommitInfo
instance ToJSON CommitInfo

instance Show CommitInfo where
  show info = (show $ commitIndex info) ++ ". " ++ (commitMessage info)

instance Ord CommitInfo where
  (<=) a b = commitIndex a <= commitIndex b

{-|
  Strategy for merging revisions.
-}
data MergeStrategy
  = MergeLeft   -- ^ merge result is first revision
  | MergeRight  -- ^ merge result is second revision
  | MergeBoth   -- ^ merge result is revision containing concatenation of two revisions


-- Constants

commitInfoFileName :: String
commitInfoFileName = "COMMIT_INFO"

cvsFileName :: String
cvsFileName = ".cvs"


-- Get and init operations

{-|
  Get CVS repository for given path.
  Currently passed path are ignored and CVS directory for it the nearest CVS directory
  of current folder.

  Returns @Maybe@ CVS directory

  Throws:
    * @InvalidCVSRepository@ if found CVS repository is @Document@
-}
getCVSForFile :: Path -> FileSystem (Maybe File)
getCVSForFile _ = do
--  file <- getFileByPath path
--  dir <- case file of
--    Just Document{ fileParent = parent } -> getDirectoryByPathOrError (fromJust parent)
--    Just d@Directory{} -> return d
--    Nothing -> throwM NoSuchFile
  dir <- gets currentDirectory
  case findInFolder dir cvsFileName of
    Just cvs@Directory{} -> return $ Just cvs
    Just Document{} -> throwM InvalidCVSRepository
    Nothing ->
      case (fileParent dir) of
        "/" :| [] -> return Nothing
        parent -> do
          liftIO $ print parent
          getCVSForFile parent

{-|
  Get CVS repository of given file or throw error.

  Returns CVS directory.

  Throws:
    * @CVSDoesNotExist@ if file does not belong to any CVS.
    * @InvalidCVSRepository@ if found CVS repository is @Document@
-}
getCVSForFileOrError :: Path -> FileSystem File
getCVSForFileOrError path = do
  maybeCvs <- getCVSForFile path
  case maybeCvs of
    Just cvs@Directory{} -> return cvs
    Just Document{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM CVSDoesNotExist

{-|
  Initialize CVS in given directory.

  Returns created CVS repository file.

  Throws:
    * @DirectoryExpected@ if path is not directory
    * @CVSAlreadyExists@ if path is already under CVS
    * @Filesystem.createFileByName@ errors
-}
cvsInit :: Path -> FileSystem File
cvsInit path = do
  _ <- getDirectoryByPathOrError path
  existing <- getCVSForFile path
  case existing of
    Just cvs -> throwM $ CVSAlreadyExists (pathToString $ filePath cvs)
    Nothing -> createFileByName path cvsFileName emptyDirectory False


-- CVS add operations

{-|
  Add file to CVS.
  If given file is a directory then all documents in its subdirectories are added.
  Created revision has index 0.

  Returns created revision directory.

  Throws:
    * @NoSuchFile@ if the file does not exist
    * @CVSDoesNotExist@ if file does not belong to any CVS
    * @FileAlreadyExists@ if file was added to CVS already
    * @Filesystem.createFileByName@ errors
-}
cvsAdd :: Path -> FileSystem File
cvsAdd path = getFileByPathOrError path >>= cvsAddFile

cvsAddFile :: File -> FileSystem File
cvsAddFile file@Document{ filePath = path } = do
  createCVSRevisionDir path
  createNewRevision path 0 file "Initial revision"
cvsAddFile dir@Directory{} = do
  foldr ((>>) . cvsAddFile) (return ()) (getAllFilesInSubDirectories dir)
  return emptyDirectory


-- CVS update operations

{-|
  Create new revision of a file with given comment.
  Revision index is incremented latest revision index.

  Returns created revision directory.

  Throws:
    * @CVSDoesNotExist@ if file does not belong to any CVS
    * @NoSuchFile@ if the file does not exist
    * @DocumentExpected@ on attempt to update directory
    * @FileNotAddedToCVS@ if file not added to CVS
-}
cvsUpdate :: Path -> String -> FileSystem File
cvsUpdate path comment = getFileByPathOrError path >>= flip cvsUpdateFile comment

cvsUpdateFile :: File -> String -> FileSystem File
cvsUpdateFile file@Document{ filePath = path } comment = do
  newRevIndex <- (+1) <$> getLatestRevisionIndex path
  createNewRevision path newRevIndex file comment
cvsUpdateFile Directory{} _ = throwM DocumentExpected

createCVSRevisionDir :: Path -> FileSystem ()
createCVSRevisionDir path = do
  cvsDir <- getCVSForFileOrError path
  hash <- revisionHash path
  maybeRevDir <- getRevisionDir path
  case maybeRevDir of
    Nothing -> void $ createFileByName (filePath cvsDir) hash emptyDirectory False
    Just _ -> return ()

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
  getDirectoryByPathOrError (filePath newRevDir)
    
revisionHash :: Path -> FileSystem String
revisionHash path = toAbsoluteFSPath path >>= return . pathHash

-- Get CVS revision operations

{-|
  Get revision for a file by its index assuming that file added to VCS..
  
  Returns @Maybe@ revision with given index.
  
  Throws:
    * @CVSDoesNotExist@ if file does not belong to any CVS
    * @FileNotAddedToCVS@ if file is not added to CVS
    * @InvalidCVSRevisionDirectory@ if revision directory is document
-}
getRevision :: Path -> Int -> FileSystem (Maybe File)
getRevision path index = do
  cvsDir <- getRevisionDirOrError path
  return $ findInFolder cvsDir (show index)

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

getLatestRevisionIndex :: Path -> FileSystem Int
getLatestRevisionIndex path = do
  revDir <- getRevisionDirOrError path
  let revisions = mapMaybe (readMaybeInt . fileName) (directoryContents revDir)
  if (null revisions)
    then return (-1)
    else return $ maximum revisions
    
{-|
  Get revision for a file by its index assuming that file added to VCS..
  
  Returns revision with given index.
  
  Throws:
    * @CVSDoesNotExist@ if file does not belong to any CVS
    * @FileNotAddedToCVS@ if file is not added to CVS
    * @UnknownRevision@ if revision not found
    * @InvalidCVSRevisionDirectory@ if revision directory is document
-}
getRevisionOrError :: Path -> Int -> FileSystem File
getRevisionOrError path index = do
  maybeRevision <- getRevision path index
  case maybeRevision of
    Just revision -> return revision
    Nothing -> throwM UnknownRevision

{-|
  Get all revisions of all files in directory and its subdirectories.
  
  Returns list of list of revisions of all files thatin this directory and its subdirectories. 
  
  Throws:
    * @CVSDoesNotExist@ if file does not belong to any CVS
    * @DirectoryExpected@ if @Document@ is given
    * @InvalidCVSRevisionDirectory@ if revision directory is document
-}
getAllRevisionsOfDirectory :: File -> FileSystem [[File]]
getAllRevisionsOfDirectory dir@Directory{} = do
  addedToCvs <- filterAddedToCvs $ getAllFilesInSubDirectories dir
  traverse getAllRevisionsOfDocument addedToCvs
getAllRevisionsOfDirectory Document{} = throwM DirectoryExpected

filterAddedToCvs :: [File] -> FileSystem [File]
filterAddedToCvs list = catMaybes <$> traverse mapFunc list
  where
    mapFunc file = do
      dir <- getRevisionDir $ filePath file
      return $ (file <$ dir)

{-|
  Get revision of a document.
  
  Returns list of all revisions of all files in this directory and subdirectories. 
  
  Throws:
    * @CVSDoesNotExist@ if file does not belong to any CVS
    * @DocumentExpected@ if @Directory@ is given
    * @FileNotAddedToCVS@ if file is not added to CVS
    * @InvalidCVSRevisionDirectory@ if revision directory is document
-}
getAllRevisionsOfDocument :: File -> FileSystem [File]
getAllRevisionsOfDocument Document{} = throwM DocumentExpected 
getAllRevisionsOfDocument dir =
  getRevisionDirOrError (filePath dir) >>=
  return . directoryContents


-- CVS removal operations

{-|
  Remove file from CVS.
  
  Throws:
    * @CVSDoesNotExist@ if file does not belong to any CVS
    * @DocumentExpected@ if @Directory@ is given
    * @FileNotAddedToCVS@ if file is not added to CVS
    * @InvalidCVSRevisionDirectory@ if revision directory is document
-}
removeFromCVS :: Path -> FileSystem ()
removeFromCVS path = do
  _ <- getDocumentByPathOrError path
  cvsDir <- getRevisionDirOrError path
  removeFile $ filePath cvsDir

{-|
  Remove certain revision of a document from CVS.
  
  Throws:
    * @CVSDoesNotExist@ if file does not belong to any CVS
    * @DocumentExpected@ if @Directory@ is given
    * @FileNotAddedToCVS@ if file is not added to CVS
    * @InvalidCVSRevisionDirectory@ if revision directory is document
-}
removeRevision :: Path -> Int -> FileSystem ()
removeRevision path index = do
  _ <- getDocumentByPathOrError path
  revision <- getRevisionOrError path index
  removeFile $ filePath revision


-- CVS revision info

{-|
  Get commit information from given revision directory.
  
  Throws:
    * @InvalidCVSRevisionDirectory@ if there is no COMMIT_INFO file in revision directory
    * @MalformedCommitInfo@ if COMMIT_INFO contains invalid data
-}
getCommitInfo :: File -> FileSystem CommitInfo
getCommitInfo revisionDir = do
  case findInFolder revisionDir commitInfoFileName of
    Just info@Document{} -> deserializeCommitInfo (documentContent info)
    Just Directory{} -> throwM InvalidCVSRevisionDirectory
    Nothing -> throwM InvalidCVSRevisionDirectory

{-|
  Extract file from revision directory
  
  Throws:
    * @InvalidCVSRevisionDirectory@ if there is no COMMIT_INFO file in revision directory
        or if there is no file found on path from COMMIT_INFO file
    * @MalformedCommitInfo@ if COMMIT_INFO contains invalid data
-}
getFileFromRevision :: File -> FileSystem File
getFileFromRevision revisionDir = do
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

deserializeCommitInfo :: BS.ByteString -> FileSystem CommitInfo
deserializeCommitInfo content =
  case eitherDecode content of
    Right info -> return info
    Left _ -> throwM MalformedCommitInfo

serializeCommitInfo :: CommitInfo -> BS.ByteString
serializeCommitInfo = encode


-- CVS merge

{-|
  Merge two revisions of a file, create new revision with merge result and replace file
  in filesystem with that new revision.
  
  Merging is happening by strategy as described in @MergeStrategy@ constructors.
  
  Throws:
    * @InvalidCVSRevisionDirectory@ if there is no COMMIT_INFO file in revision directory
        or if there is no file found on path from COMMIT_INFO file
    * @MalformedCommitInfo@ if COMMIT_INFO contains invalid data
    * @cvsUpdate@ errors
-}
mergeRevisions :: File -> File -> MergeStrategy -> FileSystem File
mergeRevisions revision1 revision2 strategy = do
  content1 <- BS.lines . documentContent <$> getFileFromRevision revision1
  content2 <- BS.lines . documentContent <$> getFileFromRevision revision2
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
  newFile <- getFileFromRevision newRevDir
  let newPath = filePath newFile
  _ <- createFile newPath newFile{ documentContent = newContent } True
  let fsFile = newFile {
      filePath = path
    , fileParent = getParentPath path
    , documentContent = newContent
  }
  _ <- createFile path fsFile True

  getDirectoryByPathOrError $ filePath newRevDir

mergeBoth :: [Diff [BS.ByteString]] -> [BS.ByteString]
mergeBoth diff = concatMap mergeFunc diff
  where
    mergeFunc (First a) = a
    mergeFunc (Both a _) = a
    mergeFunc (Second b) = b
