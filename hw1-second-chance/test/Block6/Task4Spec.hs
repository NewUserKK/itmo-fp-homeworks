module Block6.Task4Spec where

import Test.Hspec
import Block6.Task1
import Block6.Task4

parseListList :: String -> Maybe ([[Int]], String)
parseListList = runParser listListParser

spec :: Spec
spec = do
  describe "Block6.Task4.listListParser" $ do
    it "github example" $ do
      parseListList "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([[1, 10], [5, -7, 2]], "")
