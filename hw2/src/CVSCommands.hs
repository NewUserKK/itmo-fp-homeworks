module CVSCommands where

import Filesystem
import CVS
import Path
import File
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Catch (throwM)

cvsInit :: StringPath -> FileSystem File
cvsInit = CVS.cvsInit . stringToPath
    
cvsAdd :: StringPath -> FileSystem ()
cvsAdd = CVS.cvsAdd . stringToPath

cvsUpdate :: StringPath -> String -> FileSystem ()
cvsUpdate = CVS.cvsUpdate . stringToPath

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
  maybeRevision <- getCVSRevision path index
  case maybeRevision of
    Just revision -> do
      file <- getFileFromRevisionDir revision
      return $ documentContent file
    Nothing -> throwM UnknownRevision
