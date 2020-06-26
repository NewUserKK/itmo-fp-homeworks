module Task6Spec where

import Task5.FS (FS(..))
import Task6
import FSMock
import Test.Hspec
import Lens.Micro

spec :: Spec
spec = do
  describe "Task6.cd" $ do
    it "should correctly change directory" $ do
      root ^? cd "dir1" . cd "dir1_1" `shouldBe` Just dir1_1
    it "should return Nothing if dir does not exits" $ do
      root ^? cd "unknown" `shouldBe` Nothing
    it "should return Nothing if trying to cd to file" $ do
      root ^? cd "file1" `shouldBe` Nothing
  describe "Task6.ls" $ do
    it "should list contents" $ do
      root ^.. ls `shouldBe` (map _name $ _contents root)
    it "should return empty list on file" $ do
      file1 ^.. ls `shouldBe` []
  describe "Task6.file" $ do
    it "should return name if exists" $ do
      root ^? file "file1" `shouldBe` Just (_name file1)
    it "should return Nothing if file does not exist" $ do
      root ^? file "chto" `shouldBe` Nothing
  describe "Task6.integration" $ do
    it "cd + file" $ do
      (root ^? cd "dir1" . cd "dir1_1" . file "file1_1_1") `shouldBe` (Just $ _name file1_1_1)
    it "cd + ls" $ do
      (root ^.. cd "dir1" . cd "dir1_1" . ls) `shouldBe` (map _name $ _contents dir1_1)
