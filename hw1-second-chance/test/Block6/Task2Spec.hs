{-# LANGUAGE ScopedTypeVariables #-}

module Block6.Task2Spec where

import Data.Char (isDigit)
import Test.Hspec
import Test.QuickCheck
import Block6.Task1
import Block6.Task2

spec :: Spec
spec = do
  describe "Block6.Task2.ok" $ do
    it "always succeeds" $ do
      property $ \(s :: String) -> runParser ok s == Just ((), s)

  describe "Block6.Task2.eof" $ do
    it "succeeds at empty stream" $ do
      runParser eof "" `shouldBe` Just ((), "")
    it "succeeds at empty stream only" $ do
      property $ \(s :: String) ->
        runParser eof s ==
          case s of
            "" ->  Just ((), "")
            _ -> Nothing

  describe "Block6.Task2.satisfy" $ do
    it "succeeds when predicate is true" $ do
      runParser (satisfy isDigit) "1o1" `shouldBe` Just ('1', "o1")
    it "always succeeds at true predicate" $ do
      property $ \(_ :: Int) ->
        runParser (satisfy $ const True) "abc" `shouldBe` Just ('a', "bc")
    it "always fails at false predicate" $ do
      property $ \(_ :: Int) ->
        runParser (satisfy $ const False) "abc" `shouldBe` Nothing
   
  describe "Block6.Task2.element" $ do
    it "succeeds when starts with provided element" $ do
      runParser (element '2') "2ez4rtz" `shouldBe` Just ('2', "ez4rtz")
    it "fails when not starts with provided element" $ do
      runParser (element 'l') "kek" `shouldBe` Nothing
  
  describe "Block6.Task2.stream" $ do
    it "succeeds when starts with provided stream" $ do
      runParser (stream "420") "420 dorritos" `shouldBe` Just ("420", " dorritos")
    it "fails when not starts with provided stream" $ do
      runParser (stream [1 :: Int, 2, 3]) [2, 3, 1] `shouldBe` Nothing
      
      
  -- tests for additional combinators
       
  describe "Block6.Task2.eps" $ do
    it "always succeeds" $ do
      property $ \(s :: String) -> runParser eps s == Just ("", s)     
       
  describe "Block6.Task2.eol" $ do
     it "succeeds at empty stream" $ do
       runParser eol "" `shouldBe` Just ("", "")
     it "succeeds at empty stream only" $ do
       property $ \(s :: String) ->
         runParser eol s ==
           case s of
             "" ->  Just ("", "")
             _ -> Nothing
  
  describe "Block6.Task2.(+++)" $ do
    it "sequentially applies parsers with mconcat of result" $ do
      let expected = Just ("covid-2019", " sucks") in
       let actual = runParser (stream "covid" +++ stream "-2019") "covid-2019 sucks" in
       expected `shouldBe` actual
       
  describe "Block6.Task2.(+:+)" $ do
    it "sequentially applies parsers with collecting to list" $ do
      runParser (element '1' +:+ stream "11") "111" `shouldBe` Just ("111", "")
  
  describe "Block6.Task2.matchExactly" $ do
    it "matches exactly n elements of stream" $ do
      runParser (matchExactly 3 (satisfy isDigit)) "3124asd" `shouldBe` Just ("312", "4asd")