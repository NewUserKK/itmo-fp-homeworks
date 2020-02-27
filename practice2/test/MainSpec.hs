module MainSpec
  ( spec
  ) where

import Main (FName)
import Test.Hspec

spec :: Spec
spec = do
  describe "Main.FName" $ do
    it "equal strings with numeric prefixes are equal" $ do 
      FName "01abcd" == FName "01abcd" `shouldBe` True
    it "equal strings without numeric prefixes are equal" $ do 
      FName "abcd" == FName "abcd" `shouldBe` True
    it "equal strings with zero-leading numeric prefixes are equal" $ do 
      FName "01abcd" == FName "1aBCd" `shouldBe` True
    it "strings with zero-leading numeric prefixes compare correcly" $ do 
      FName "01aab" < FName "1bbc" `shouldBe` True
    it "strings with numeric prefixes compare correcly" $ do 
      FName "12aab" > FName "1BBc" `shouldBe` True
    it "one string without numeric prefix and other with compare correctly" $ do
      FName "12Aab" < FName "b" `shouldBe` True
    it "strings without numeric prefixes compare correcly" $ do 
      FName "AAB" < FName "bbc" `shouldBe` True
