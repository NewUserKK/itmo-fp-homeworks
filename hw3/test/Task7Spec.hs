module Task7Spec where

import Data.List (sort)
import Test.Hspec
import Task7
import FSMock
import Lens.Micro
import Task5.FS

spec :: Spec
spec = do
  describe "Task7.changeExtensions" $ do
    it "changes extensions of all files" $ do
      let expected = dir1_1 { _contents = [file1_1_1 {_name = "file1_1_1.txt"}]}
      let actual = changeExtensions "txt" dir1_1
      actual `shouldBe` expected
  describe "Task7.allFiles" $ do
    it "returns all files recusively" $ do
      let expected = ["/", "dir1", "dir1_1", "dir2", "file1", "file1_1", "file1_1_1"]
      let actual = allFiles root
      sort actual `shouldBe` sort expected
  describe "Task7.removeIfEmpty" $ do
    it "removes folder if it is empty" $ do
      let expected = root { _contents = [dir1, file1] }
      let actual = removeIfEmpty "dir2" root
      actual `shouldBe` expected
    it "does not remove folder if it is not empty" $ do
      let expected = root
      let actual = removeIfEmpty "dir1" root
      actual `shouldBe` expected
    it "does not remove file" $ do
      let expected = root
      let actual = removeIfEmpty "file1" root
      actual `shouldBe` expected
  describe "Task7.getPath" $ do
    it "gets correct full path" $ do
      let expected = "//dir1/dir1_1"
      let actual = root ^. move "dir1" . move "dir1_1" . getPath
      actual `shouldBe` expected
