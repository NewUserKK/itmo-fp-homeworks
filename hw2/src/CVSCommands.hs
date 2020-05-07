module CVSCommands where

import Filesystem
import CVS
import Path
import File
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Catch (throwM)
import Control.Monad (void)

cvsInit :: StringPath -> FileSystem File
cvsInit = CVS.cvsInit . stringToPath
    
cvsAdd :: StringPath -> FileSystem ()
cvsAdd = void . CVS.cvsAdd . stringToPath

cvsUpdate :: StringPath -> String -> FileSystem ()
cvsUpdate path comment = void $ CVS.cvsUpdate (stringToPath path) comment

cvsHistoryForDocument :: File -> FileSystem [CommitInfo]
cvsHistoryForDocument doc@Document{} =
  traverse getCommitInfo =<< getAllRevisionsOfDocument doc
cvsHistoryForDocument Directory{} = throwM DocumentExpected

cvsHistoryForDirectory :: File -> FileSystem [[CommitInfo]]
cvsHistoryForDirectory dir@Directory{} = do
  revisions <- getAllRevisionsOfDirectory dir
  traverse (traverse getCommitInfo) revisions
cvsHistoryForDirectory Document{} = throwM DirectoryExpected

cvsShow :: StringPath -> Int -> FileSystem BS.ByteString
cvsShow stringPath index = do
  let path = stringToPath stringPath
  revision <- getCVSRevisionOrError path index
  file <- getFileFromRevisionDir revision
  return $ documentContent file

cvsRemove :: StringPath -> FileSystem ()
cvsRemove = removeFromCVS . stringToPath

cvsRemoveRevision :: StringPath -> Int -> FileSystem ()
cvsRemoveRevision = removeRevision . stringToPath

cvsMergeRevisions :: StringPath -> Int -> Int -> MergeStrategy -> FileSystem File
cvsMergeRevisions stringPath index1 index2 strategy = do
  let path = stringToPath stringPath
  revision1 <- getCVSRevisionOrError path index1
  revision2 <- getCVSRevisionOrError path index2
  mergeRevisions revision1 revision2 strategy