module Block5.Task1Spec where

import Test.Hspec
import Block5.Task1

spec :: Spec
spec = do
  describe "Block5.Task1.evalExpr" $ do
    it "Const should return its value" $ do
      evalExpr (Const (4 :: Int)) `shouldBe` Right 4
    it "Add should add numbers" $ do
      evalExpr (Add (Const (2 :: Int)) (Const 3)) `shouldBe` Right 5
    it "Sub should subtract numbers" $ do
      evalExpr (Sub (Const (2 :: Int)) (Const 3)) `shouldBe` Right (-1)
    it "Mul should multiply numbers" $ do
      evalExpr (Mul (Const (-2 :: Int)) (Const 3)) `shouldBe` Right (-6)
    describe "Div" $ do
      it "should divide numbers" $ do
        evalExpr (Div (Const (6 :: Int)) (Const 3)) `shouldBe` Right 2
      it "division should be integral" $ do
        evalExpr (Div (Const (7 :: Int)) (Const 3)) `shouldBe` Right 2
      it "division by zero should return DVZ error" $ do
        evalExpr (Div (Const (7 :: Int)) (Const 0)) `shouldBe` Left DivisionByZero
    describe "Pow" $ do
      it "should pow numbers" $ do
        evalExpr (Pow (Const (2 :: Int)) (Const 4)) `shouldBe` Right 16
      it "should return error in case of negative power" $ do
        evalExpr (Pow (Const (2 :: Int)) (Const (-4))) `shouldBe` Left NegativePow
      it "should return correct result in case of 0 ^ 0" $ do
        evalExpr (Pow (Const (0 :: Int)) (Const 0)) `shouldBe` Right 1
