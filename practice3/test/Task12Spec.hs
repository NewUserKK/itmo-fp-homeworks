{-# LANGUAGE ScopedTypeVariables #-}

module Task12Spec
  ( spec
  ) where

import           Task12
import           Test.Hspec

spec :: Spec
spec = do
  describe "Task12.createAbs" $ do
    it "absolute path should work correctly" $ do
      let expected = Just $ Path $ ["home", "newuserkk", "functional-programming"]
          actual = createAbs "/home/newuserkk/functional-programming"
        in
          actual `shouldBe` expected 
    it "relative path should return Nothing" $ do
        let expected = Nothing
            actual = createAbs "home/newuserkk/functional-programming"
          in
            actual `shouldBe` expected

  describe "Task12.createRel" $ do
    it "relative path should work correctly" $ do
      let expected = Just $ Path $ ["newuserkk", "functional-programming"]
          actual = createRel "newuserkk/functional-programming"
        in
          actual `shouldBe` expected 
    it "relative path should work correctly with beginning dot" $ do
      let expected = Just $ Path $ [".", "newuserkk", "functional-programming"]
          actual = createRel "./newuserkk/functional-programming"
        in
          actual `shouldBe` expected 
    it "relative path should return Nothing" $ do
        let expected = Nothing
            actual = createRel "/home/newuserkk/functional-programming"
          in
            actual `shouldBe` expected
