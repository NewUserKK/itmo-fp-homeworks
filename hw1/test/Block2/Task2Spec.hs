module Block2.Task2Spec where

import Block2.Task2
import Data.List.NonEmpty
import Test.Hspec

spec :: Spec
spec = do
  describe "Block2.Task2.splitOn" $ do
    it "should split correctly" $ do
      splitOn '/' "home/newuserkk/fp-homeworks" `shouldBe` "home" :| ["newuserkk", "fp-homeworks"]
    it "empty result with empty input" $ do
      splitOn (0 :: Int) [] `shouldBe` [] :| []
    it "separator in the beginning" $ do
      splitOn (0 :: Int) [0, 1, 2, 0, 3] `shouldBe` [] :| [[1, 2], [3]]
    it "separator in the end" $ do
      splitOn (0 :: Int) [1, 2, 0] `shouldBe` [1, 2] :| [[]]
    it "consequent separators" $ do
      splitOn (0 :: Int) [1, 0, 0, 2] `shouldBe` [1] :| [[], [2]]
