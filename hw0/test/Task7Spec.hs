module Task7Spec
  ( spec
  ) where

import Task7 (firstSubtask, secondSubtask, thirdSubtask)
import Test.Hspec

spec :: Spec
spec = do
  describe "Task7.firstSubtask" $ do
    it "return correct result with annotated types" $ do 
        firstSubtask `shouldBe` False
  describe "Task7.secondSubtask" $ do
    it "return correct result with annotated types" $ do 
        secondSubtask `shouldBe` [(3, 64)]
  describe "Task7.thirdSubtask" $ do
    it "returns True on all inputs" $ do 
        all thirdSubtask [-100..100] `shouldBe` True
