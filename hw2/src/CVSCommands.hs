module CVSCommands where

import Filesystem
import CVS
import Path
import File
import Control.Monad.State

cvsInit :: StringPath -> FileSystem ()
cvsInit stringPath = do
  created <- CVS.cvsInit (stringToPath stringPath)
  liftIO $ putStrLn $
    "Initialized empty CVS repository at " ++ (pathToString . filePath $ created)