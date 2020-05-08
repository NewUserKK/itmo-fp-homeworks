module CVSCommands where

import Control.Monad (void)
import Control.Monad.Catch (throwM)
import CVSCore
import qualified Data.ByteString.Lazy as BS
import File
import FilesystemCore
import Path

cvsInit :: StringPath -> FileSystem File
cvsInit = CVSCore.cvsInit . stringToPath

cvsAdd :: StringPath -> FileSystem ()
cvsAdd = void . CVSCore.cvsAdd . stringToPath

cvsUpdate :: StringPath -> String -> FileSystem ()
cvsUpdate path comment = void $ CVSCore.cvsUpdate (stringToPath path) comment

cvsHistoryForDocument :: File -> FileSystem [CommitInfo]
cvsHistoryForDocument doc@Document{} =
  getAllRevisionsOfDocument doc >>= traverse getCommitInfo
cvsHistoryForDocument Directory{} = throwM DocumentExpected

cvsHistoryForDirectory :: File -> FileSystem [[CommitInfo]]
cvsHistoryForDirectory dir@Directory{} = do
  revisions <- getAllRevisionsOfDirectory dir
  traverse (traverse getCommitInfo) revisions
cvsHistoryForDirectory Document{} = throwM DirectoryExpected

cvsShow :: StringPath -> Int -> FileSystem BS.ByteString
cvsShow stringPath index = do
  let path = stringToPath stringPath
  revision <- getRevisionOrError path index
  file <- getFileFromRevision revision
  return $ documentContent file

cvsRemove :: StringPath -> FileSystem ()
cvsRemove = removeFromCVS . stringToPath

cvsRemoveRevision :: StringPath -> Int -> FileSystem ()
cvsRemoveRevision = removeRevision . stringToPath

cvsMergeRevisions :: StringPath -> Int -> Int -> MergeStrategy -> FileSystem File
cvsMergeRevisions stringPath index1 index2 strategy = do
  let path = stringToPath stringPath
  revision1 <- getRevisionOrError path index1
  revision2 <- getRevisionOrError path index2
  mergeRevisions revision1 revision2 strategy
