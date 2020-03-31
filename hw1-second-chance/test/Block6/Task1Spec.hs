module Block6.Task1Spec where

import Block6.Task1
import Control.Applicative
import Test.Hspec

parser :: Parser Char String
parser = Parser $ \s -> Just ("1", s)

failingParser :: Parser Char String
failingParser = Parser $ \_ -> Nothing

spec :: Spec
spec = do
  describe "Block6.Task1.Functor" $ do
    it "fmap works correctly" $ do
      runParser ((++ "2") <$> parser) "a" `shouldBe` Just ("12", "a")

  describe "Block6.Task1.Applicative" $ do
    it "<*> works correctly" $ do
      runParser ((++) <$> parser <*> parser) "a" `shouldBe` Just ("11", "a")
    it "pure works correctly" $ do
      runParser (pure "2") "a" `shouldBe` Just ("2", "a")

  describe "Block6.Task1.Monad" $ do
    it "bind works correctly" $ do
      runParser (parser >>= \res -> (++ "2") <$> pure res) "a" `shouldBe` Just ("12", "a")

  describe "Block6.Task1.Alternative" $ do
    it "<|> works correctly" $ do
      runParser (parser <|> failingParser) "a" `shouldBe` Just ("1", "a")
    it "empty works correctly" $ do
      runParser (parser <|> empty) "a" `shouldBe` Just ("1", "a")

