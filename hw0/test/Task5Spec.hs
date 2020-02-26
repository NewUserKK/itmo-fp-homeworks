module Task5Spec
  ( spec
  ) where

import Task5 (Nat, churchMult, churchPlus, churchToInt, succChurch, zero)
import Test.Hspec

one :: Nat a
one = succChurch zero

two :: Nat a
two = succChurch one

three :: Nat a
three = succChurch two

spec :: Spec
spec = do
  describe "Task5.churchToInt" $ do
    it "churchZero returns zero" $ do
      churchToInt zero `shouldBe` 0
    it "increments correctly" $ do
      churchToInt (succChurch zero) `shouldBe` 1 + churchToInt zero
    it "adds correctly" $ do
      churchToInt (churchPlus one two) `shouldBe` churchToInt one + churchToInt two
    it "multiplies correctly" $ do
      churchToInt (churchMult two three) `shouldBe` churchToInt two * churchToInt three
