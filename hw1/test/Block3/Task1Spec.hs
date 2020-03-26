module Block3.Task1Spec where

import Test.Hspec
import Block3.Task1

spec :: Spec
spec = do
  describe "Block3.Task1.maybeConcat" $ do
    it "should concat inner lists" $ do
      maybeConcat [Just [1 :: Int, 2, 3], Nothing, Just [4, 5]] `shouldBe` [1, 2, 3, 4, 5]
    it "should return empty list when input contains only Nothing" $ do
      maybeConcat [Nothing :: Maybe [Int], Nothing, Nothing] `shouldBe` []
    it "should return empty list when input is empty" $ do
      maybeConcat ([] :: [Maybe [Int]]) `shouldBe` []
