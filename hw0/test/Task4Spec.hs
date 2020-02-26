module Task4Spec
  ( spec
  ) where

import Task4 (factorial, fibonacci, iterateElement, mapFix)
import Test.Hspec

testIterate :: IO ()
testIterate = (actual :: [Integer]) `shouldBe` (expected :: [Integer])
  where
    expected = take 100 (iterateElement 2)
    actual = take 100 [2,2 ..]

testMapFix :: IO ()
testMapFix = (actual :: [Integer]) `shouldBe` (expected :: [Integer])
  where
    actual = mapFix operation values
    expected = map operation values
    operation = (* 2)
    values = [1, 2, 3, 4, 5]

spec :: Spec
spec = do
  describe "Task4.iterateElement" $ do
    it "returns infinite list of the same elements" $ do 
      testIterate
      
  describe "Task4.fibonacci" $ do
    it "returns correct result for positive numbers" $ do
      fibonacci 7 `shouldBe` 13
    it "returns correct result for the zeroth element" $ do
      fibonacci 0 `shouldBe` 0
    it "returns correct result for the first element" $ do
      fibonacci 1 `shouldBe` 1
    it "returns correct result for the second element" $ do
      fibonacci 2 `shouldBe` 1

  describe "Task4.factorial" $ do
    it "returns correct result for positive numbers" $ do
      factorial 5 `shouldBe` 120
    it "returns correct result for zero" $ do 
      factorial 0 `shouldBe` 1

  describe "Task4.mapFix" $ do
    it "returns correctly transformed list" $ do
       testMapFix
