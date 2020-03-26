module Block4.Task1Spec where

import Test.Hspec
import Block4.Task1

spec :: Spec
spec = do
  describe "Block4.Task1.stringSum" $ do
    it "should calculate sum with correct input" $ do
      stringSum "420 228 13 -300 123 -37 42 -420" `shouldBe` Just 69
    it "should return Nothing if input contains not numbers" $ do
      stringSum "2 4 8 serokell 16 32 64" `shouldBe` Nothing
    it "should return Nothing on empty input" $ do
      stringSum "" `shouldBe` Nothing
