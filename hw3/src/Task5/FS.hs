module Task5.FS where

import Lens.Micro

data FS
  = Dir
      { _name :: FilePath -- название папки, не полный путь
      , _contents :: [FS]
      }
  | File
      { _name :: FilePath -- название файла, не полный путь
      }
  deriving (Show, Eq)

name :: Lens' FS FilePath
name = lens _name (\fs newName -> fs { _name = newName })

contents :: Lens' FS [FS]
contents = lens _contents (\fs newContents -> fs { _contents = newContents })

_Dir :: Traversal' FS FS
_Dir f dir@(Dir{}) = f dir
_Dir _ file = pure file

_File :: Traversal' FS FS
_File f file@(File{}) = f file
_File _ dir = pure dir
