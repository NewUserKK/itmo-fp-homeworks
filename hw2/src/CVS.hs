module CVS where

import Path
import File
import Filesystem as FS
import Control.Monad.State

cvsInit :: Path -> FileSystem File
cvsInit path = FS.createFileByName path ".cvs" (emptyDirectory path) False
