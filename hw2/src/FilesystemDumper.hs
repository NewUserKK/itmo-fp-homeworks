module FilesystemDumper where

import Data.List.NonEmpty as NE
import Filesystem (FileSystem, FSState(..))
import Path
import Control.Monad.State
import System.Directory

dumpFilesystem :: FSState -> IO ()
dumpFilesystem fsState = do
  let rootPath = realRootPath fsState
  setCurrentDirectory rootPath
  let backupPath = rootPath </> ".fm_old"
  removePathForcibly backupPath
  copyDir rootPath backupPath

  return ()

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dest = do
  createDirectory dest
  content <- Prelude.filter (`notElem` [".", ".."]) <$> getDirectoryContents src
  forM_ content $ \name -> do
    let srcPath = src </> name
    let destPath = dest </> name
    print srcPath
    print destPath
    unless (srcPath == dest) $ do
      isDirectory <- doesDirectoryExist srcPath
      if isDirectory
        then copyDir srcPath destPath
        else copyFile srcPath destPath

toRealPath :: Path -> FileSystem FilePath
toRealPath path = do
  rootPath <- gets realRootPath
  case path of
    "/" :| [] -> return rootPath
    "/" :| [x] -> return $ rootPath ++ x
    "/" :| x : xs -> return $ rootPath <> (pathToString $ x :| xs)
    _ -> error "path expected to be absolute"
