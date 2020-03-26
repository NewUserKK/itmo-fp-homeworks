module Block1.Task1Spec
  ( spec
  ) where

import Block1.Task1
import Test.Hspec

spec :: Spec
spec = do
  describe "Task1.nextDay" $ do
    it "returns correct next day" $ do 
      nextDay Wednesday `shouldBe` Thursday 
    it "returns correct next day after Sunday" $ do
      nextDay Sunday `shouldBe` Monday
      
  describe "Task1.afterDays" $ do
    it "returns correct result" $ do
      afterDays Monday 2 `shouldBe` Wednesday
    it "returns correct result with overflow" $ do
      afterDays Wednesday 7 `shouldBe` Wednesday

  describe "Task1.isWeekend" $ do
    it "Saturday should be weekend" $ do
      isWeekend Saturday `shouldBe` True
    it "Sunday should be weekend" $ do
      isWeekend Sunday `shouldBe` True
    it "Monday should not be weekend" $ do
      isWeekend Monday `shouldBe` False
    it "Tuesday should not be weekend" $ do
      isWeekend Tuesday `shouldBe` False
    it "Wednesday should not be weekend" $ do
      isWeekend Wednesday `shouldBe` False
    it "Thursday should not be weekend" $ do
      isWeekend Thursday `shouldBe` False
    it "Friday should not be weekend" $ do
      isWeekend Friday `shouldBe` False

  describe "Task1.daysToParty" $ do
    it "returns correct result for Monday" $ do
      daysToParty Monday `shouldBe` 4
    it "returns correct result for Tuesday" $ do
      daysToParty Tuesday `shouldBe` 3
    it "returns correct result for Wednesday" $ do
      daysToParty Wednesday `shouldBe` 2
    it "returns correct result for Thursday" $ do
      daysToParty Thursday `shouldBe` 1
    it "returns correct result for Friday" $ do
      daysToParty Friday `shouldBe` 0
    it "returns correct result for Saturday" $ do
      daysToParty Saturday `shouldBe` 6
    it "returns correct result for Sunday" $ do
      daysToParty Sunday `shouldBe` 5