module FSMock where

import Task5.FS

root :: FS
root =
  Dir
    { _name = "/"
    , _contents =
      [ dir1
      , dir2
      , file1
    ]
  }

dir1 :: FS
dir1 =
  Dir
    { _name = "dir1"
    , _contents = [dir1_1, file1_1]
    }

dir1_1 :: FS
dir1_1 =
  Dir
    { _name = "dir1_1"
    , _contents = [file1_1_1]
    }

file1_1_1 :: FS
file1_1_1 =
  File
    { _name = "file1_1_1"
    }

file1_1 :: FS
file1_1 =
  File
    { _name = "file1_1"
    }

dir2 :: FS
dir2 =
  Dir
    { _name = "dir2"
    , _contents = []
    }

file1 :: FS
file1 =
  File
    { _name = "file1"
    }