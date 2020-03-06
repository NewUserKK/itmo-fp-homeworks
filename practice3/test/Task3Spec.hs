{-# LANGUAGE ScopedTypeVariables #-}

module Task3Spec
  ( spec
  ) where

import Task3
import Test.Hspec
import Data.Monoid

spec :: Spec
spec = do
  describe "Task3.foldMap" $ do
    it "should work correctly" $ do
      actual `shouldBe` expected 
        where
          expected = Prelude.foldMap op foldable
          actual = Task3.foldMap op foldable
          op :: Integer -> Sum Integer = \x -> Sum (3 + x)
          foldable :: [Integer]  = [1, 2, 3]
