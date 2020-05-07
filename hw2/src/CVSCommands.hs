module CVSCommands where

import Filesystem
import CVS
import Path
import File

cvsInit :: StringPath -> FileSystem File
cvsInit = CVS.cvsInit . stringToPath
    
cvsAdd :: StringPath -> FileSystem ()
cvsAdd = CVS.cvsAdd . stringToPath

cvsUpdate :: StringPath -> String -> FileSystem ()
cvsUpdate = CVS.cvsUpdate . stringToPath