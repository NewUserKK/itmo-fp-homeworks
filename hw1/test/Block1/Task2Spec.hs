module Block1.Task2Spec
  ( spec
  ) where

import Prelude hiding (fromInteger, toInteger, div, mod)
import Block1.Task2
import Test.Hspec

one :: Nat
one = S Z

two :: Nat
two = S one

three :: Nat
three = S two

spec :: Spec
spec = do
  describe "Task2.(+)" $ do
    it "associativity law" $ do 
      (one + two) + three `shouldBe` one + (two + three) 
    it "commutativity law" $ do
      one + three `shouldBe` three + one
    it "neutral element" $ do
      two + Z `shouldBe` two
      
  describe "Task2.(*)" $ do
    it "associativity law" $ do 
      (one * two) * three `shouldBe` one * (two * three) 
    it "commutativity law" $ do
      two * three `shouldBe` three * two
    it "neutral element" $ do
      two * one `shouldBe` two
    it "multply by zero" $ do
      two * Z `shouldBe` Z
    it "distributivity with respect to (+)" $ do
      (one + two) * three `shouldBe` one * three + two * three

  describe "Task2.(-)" $ do
    it "neutral element" $ do
      two - Z `shouldBe` two
    it "self - self should be zero" $ do
      two - two `shouldBe` Z

  describe "Task2.fromInteger" $ do
    it "should work correctly with zero" $ do
      fromInteger 0 `shouldBe` Z
    it "should work correctly with positive numbers" $ do
      fromInteger 3 `shouldBe` three

  describe "Task2.toInteger" $ do
    it "should work correctly with zero" $ do
      toInteger Z `shouldBe` 0
    it "should work correctly with positive numbers" $ do
      toInteger three `shouldBe` 3

  describe "Task2.Eq" $ do
    it "equal numbers are equal" $ do
      three == three `shouldBe` True
    it "not equal numbers are not equal" $ do
      three == two `shouldBe` False

  describe "Task2.Ord" $ do
    it "should work correctly" $ do
      three > two && Z <= one `shouldBe` True

  describe "Task2.isEven" $ do
    it "should work correctly for even numbers" $ do
      isEven two `shouldBe` True
    it "should work correctly for not even numbers" $ do
      isEven three `shouldBe` False
    it "zero is even" $ do
      isEven Z `shouldBe` True
--
--  describe "Task2.div" $ do
--    it "should work correctly" $ do
--      (three `div` two) `shouldBe` one
--
--  describe "Task2.mod" $ do
--    it "should work correctly" $ do
--      (three `mod` two) `shouldBe` one
-- 