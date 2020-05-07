module CVSCommands where

import Filesystem
import CVS
import Path
import File
import Control.Monad.Catch (throwM)

cvsInit :: StringPath -> FileSystem File
cvsInit = CVS.cvsInit . stringToPath
    
cvsAdd :: StringPath -> FileSystem ()
cvsAdd = CVS.cvsAdd . stringToPath

cvsUpdate :: StringPath -> String -> FileSystem ()
cvsUpdate = CVS.cvsUpdate . stringToPath

cvsHistoryForDocument :: File -> FileSystem [CommitInfo]
cvsHistoryForDocument doc@Document{} = 
  traverse getCommitInfoFromRevisionDir =<< getAllRevisionsOfDocument doc
cvsHistoryForDocument Directory{} = throwM DocumentExpected

cvsHistoryForDirectory :: File -> FileSystem [[CommitInfo]]
cvsHistoryForDirectory dir@Directory{} = do
  revisions <- getAllRevisionsOfDirectory dir
  traverse (traverse getCommitInfoFromRevisionDir) revisions
cvsHistoryForDirectory Document{} = throwM DirectoryExpected