module Block6.Task3Spec where

import Test.Hspec
import Block6.Task1
import Block6.Task3

parseBrackets :: String -> Maybe (String, String)
parseBrackets = runParser correctBracketSequence

parseNumber :: String -> Maybe (Int, String)
parseNumber = runParser numberParser

spec :: Spec
spec = do
  describe "Block6.Task3.rightBracketSequence" $ do
    describe "correct cases" $ do
      it "empty string" $ do
        parseBrackets "" `shouldBe` Just ("", "")
      it "()" $ do
        parseBrackets "()" `shouldBe` Just ("()", "")
      it "()()" $ do
        parseBrackets "()()" `shouldBe` Just ("()()", "")
      it "(())" $ do
        parseBrackets "(())" `shouldBe` Just ("(())", "")
      it "(()())" $ do
        parseBrackets "(()())" `shouldBe` Just ("(()())", "")
      it "(()())((()()()()))(((())))" $ do
        parseBrackets "(()())((()()()()))(((())))" `shouldBe` Just ("(()())((()()()()))(((())))", "")
    describe "incorrect cases" $ do
      it "one opening bracket" $ do
        parseBrackets "(" `shouldBe` Nothing
      it "one closing bracket" $ do
        parseBrackets ")" `shouldBe` Nothing
      it "positive balance" $ do
        parseBrackets "((())" `shouldBe` Nothing
      it "negative balance" $ do
        parseBrackets "(()))" `shouldBe` Nothing
      it "incorrect brackets order" $ do
        parseBrackets ")(" `shouldBe` Nothing
      it "symbols other than brackets" $ do
        parseBrackets "()what" `shouldBe` Nothing

  describe "Block6.Task3.numberParser" $ do
    it "succeeds with single digit" $ do
      parseNumber "0" `shouldBe` Just (0, "")
    it "succeeds with multiple digits" $ do
      parseNumber "123" `shouldBe` Just (123, "")
    it "succeeds with plus at the beginning" $ do
      parseNumber "+123" `shouldBe` Just (123, "")
    it "fails with multiple plus symbols at the beginning" $ do
      parseNumber "++123" `shouldBe` Nothing
    it "ignores leading zeros" $ do
      parseNumber "0000007" `shouldBe` Just (7, "")
    it "succeeds with negative numbers" $ do
      parseNumber "-1000" `shouldBe` Just (-1000, "")
    it "fails with multiple minus signs" $ do
      parseNumber "--1000" `shouldBe` Nothing
    it "fails when it is not a number" $ do
      parseNumber "+dva" `shouldBe` Nothing