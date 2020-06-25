module Task5.PracticeSpec where

import Test.Hspec
import Task5.Practice
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
    , _contents = []
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

spec :: Spec
spec = do
  describe "Task5.lsOrEmpty" $ do
    it "should list contents of dir" $ do
      lsOrEmpty root `shouldBe` _contents root
    it "should return empty list" $ do
      lsOrEmpty file1 `shouldBe` []
  describe "Task5.maybeDirName" $ do
    it "should return name if it is dir" $ do
      maybeDirName root `shouldBe` Just "/"
    it "should return Nothing if it is file" $ do
      maybeDirName file1 `shouldBe` Nothing
  describe "Task5.fileNameOrEmpty" $ do
    it "should return name of file if it is file" $ do
      fileNameOrEmpty file1 `shouldBe` "file1"
    it "should return empty string if it is dir" $ do
      fileNameOrEmpty root `shouldBe` ""
  describe "Task5.changeRootName" $ do
    it "should return dir with changed name" $ do
      _name (changeRootName dir1) `shouldBe` "/"
    it "should not change name if it is file" $ do
      _name (changeRootName file1) `shouldBe` _name file1
  describe "Task5.addSuffix" $ do
    it "should return dir with added suffix" $ do
      _name (addSuffix "a" root) `shouldBe` "/a"
    it "should not change name if it is file" $ do
      _name (addSuffix "b" file1) `shouldBe` _name file1
  describe "Task5.firstDirName" $ do
    it "should return name of the first dir" $ do
      firstDirName root `shouldBe` (Just $ _name dir1)
    it "should return Nothing if dir is empty" $ do
      firstDirName dir1 `shouldBe` Nothing
    it "should return Nothing if it is file" $ do
      firstDirName file1 `shouldBe` Nothing
  describe "Task5.fileNames" $ do
    it "should return name of all files in dir" $ do
      fileNames root `shouldBe` ["file1"]
    it "should return empty list if it is file" $ do
      fileNames file1 `shouldBe` []
