{-# LANGUAGE ScopedTypeVariables #-}

module Task6Spec
  ( spec
  ) where

import           Task6
import           Test.Hspec

spec :: Spec
spec = do
  describe "Task6.fmap" $ do
    it "fmap should work correctly" $ do
      let actual :: Point3D Integer = fmap (+ 1) (Point3D 5 6 7)
       in 
         actual `shouldBe` (Point3D 6 7 8)
    it "<*> should work correctly" $ do
      let actual :: Point3D Integer = (Point3D (+ 1) (+ 2) (+ 3) <*> Point3D 5 6 7)
       in 
         actual `shouldBe` (Point3D 6 8 10)
