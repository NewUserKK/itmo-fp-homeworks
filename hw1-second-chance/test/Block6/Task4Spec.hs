module Block6.Task4Spec where

import Test.Hspec
import Block6.Task1
import Block6.Task4

parseListList :: String -> Maybe ([[Int]], String)
parseListList = runParser listListParser

spec :: Spec
spec = do
  describe "Block6.Task4.listListParser" $ do
    it "simple example" $ do
      parseListList "2,1,+10,3,5,-7,2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
    it "zero list size" $ do
      parseListList "0, 0, 1, 2, 0" `shouldBe` Just ([[], [], [2], []], "")
    it "only zero" $ do
      parseListList "0" `shouldBe` Just ([[]], "")
    it "empty input is empty list" $ do
      parseListList "" `shouldBe` Just ([], "")
    it "fails on negative list size" $ do
      parseListList "1, 2, -1, 1, 2" `shouldBe` Nothing
    it "ignores spaces" $ do
      parseListList "  2    ,1, +10,    3, 5,       -7,    2    " `shouldBe`
        Just ([[1, 10], [5, -7, 2]], "")
    it "fails on spaces between digits" $ do
      parseListList "1, 1 0" `shouldBe` Nothing
    it "fails on wrong separators" $ do
      parseListList "1|2|3" `shouldBe` Nothing
    it "fails when failed to parse numbers" $ do
      parseListList "2,dva,3" `shouldBe` Nothing
