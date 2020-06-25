module Task5.FilesystemLoaderSpec where

import Test.Hspec
import Task5.FilesystemLoader

spec :: Spec
spec = do
  describe "Task5.FilesystemLoader.loadFilesystem" $ do
    it "loads directory from drive if it is exist" $ do
      let path = "/home/newuserkk/[Programming]/ITMO/functional-programming/fp-homework/hw32"
      print =<< loadFilesystem path
    it "return Nothing if it is not exist" $ do
      let path = "!"
      fs <- loadFilesystem path
      fs `shouldBe` Nothing
