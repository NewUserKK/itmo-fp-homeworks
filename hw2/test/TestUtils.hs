{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import Data.Time (UTCTime(..), Day(..), secondsToDiffTime)
import Control.Monad.State
import File
import Filesystem
import Path
import Test.Hspec

toPath :: String -> Path
toPath = stringToPath

evalFS :: FileSystem a -> IO a
evalFS fs = evalStateT fs fsState

evalShouldBe :: (Eq a, Show a) => FileSystem a -> a -> Expectation
evalShouldBe actual expected = evalFS actual >>= (`shouldBe` expected)

evalShouldThrow :: (Eq a, Show a) => FileSystem a -> Expectation
evalShouldThrow actual = evalFS actual `shouldThrow` anyException

errorHandler :: (Eq a, Show a) => CommandExecutionError -> FileSystem (Maybe a)
errorHandler _ = return Nothing

emptyTime :: UTCTime
emptyTime = UTCTime
  { utctDay = ModifiedJulianDay 0
  , utctDayTime = secondsToDiffTime 0
  }

fsState :: FSState
fsState = FSState
  { rootDirectory = root
  , currentDirectory = root
  , realRootPath = "/home/newuserkk"
  }

root :: File
root = emptyDirectory
  { filePath = toPath "/"
  , directoryContents = [dir1, file1]
  }

rootWithCVS :: File
rootWithCVS = root
  { directoryContents = directoryContents root ++ [rootCVS]
  }

rootCVS :: File
rootCVS = emptyDirectory
  { filePath = toPath "/.cvs"
  , fileParent = toPath "/"
  }

dir1 :: File
dir1 = emptyDirectory
  { filePath = toPath "/dir1"
  , fileParent = toPath "/"
  , directoryContents = [dir1_dir2, dir1_file1]
  }

dir1_dir2 :: File
dir1_dir2 = emptyDirectory
  { filePath = toPath "/dir1/dir2"
  , fileParent = stringToPath "/dir1"
  , directoryContents = []
  }

file1 :: File
file1 = (emptyDocument emptyTime)
  { filePath = toPath "/file1.txt"
  , fileParent = toPath "/"
  , documentContent = "КОНТЕНТ"
  }

dir1_file1 :: File
dir1_file1 = (emptyDocument emptyTime)
  { filePath = toPath "/dir1/file1.txt"
  , fileParent = toPath "/dir1"
  , documentContent = "ModifiedJulianDay"
  }

newFile :: File
newFile = (emptyDocument emptyTime)
  { documentContent = "new content"
  }
