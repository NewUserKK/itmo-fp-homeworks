module Task8Spec where

import Task8.Simulation
import Task8.Constants
import Test.Hspec

spec :: Spec
spec = do
  describe "Task8.TODO" $ do
    it "TODO" $ do
      runWithEnvironment defaultEnv defaultStepCount
      "1" `shouldBe` "1"
