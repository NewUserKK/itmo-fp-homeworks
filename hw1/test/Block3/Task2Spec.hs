module Block3.Task2Spec where

import Test.Hspec
import Block3.Task2

thisOrThat :: ThisOrThat String String -> ThisOrThat String String
thisOrThat = id

spec :: Spec
spec = do
  describe "Block3.Task2.NonEmpty" $ do
    it "associativity law" $ do
      let leftAssoc = (((1 :: Int) :| [2, 3]) <> (4 :| [5, 6])) <> (7 :| [8, 9])
       in let rightAssoc =  (1 :| [2, 3]) <> ((4 :| [5, 6]) <> (7 :| [8, 9]))
       in leftAssoc `shouldBe` rightAssoc
       
  describe "Block3.Task2.ThisOrThat" $ do
    describe "associativity laws" $ do
      it "(This 'a1' <> This 'a2') <> This 'a3' `shouldBe` This 'a1' <> (This 'a2' <> This 'a3')" $ do
        thisOrThat ((This "a1" <> This "a2") <> This "a3") `shouldBe` This "a1" <> (This "a2" <> This "a3")
      it "(This 'a1' <> This 'a2') <> That 'b3' `shouldBe` This 'a1' <> (This 'a2' <> That 'b3')" $ do
        thisOrThat ((This "a1" <> This "a2") <> That "b3") `shouldBe` This "a1" <> (This "a2" <> That "b3")
      it "(This 'a1' <> This 'a2') <> Both 'a3' 'b3' `shouldBe` This 'a1' <> (This 'a2' <> Both 'a3' 'b3')" $ do
        thisOrThat ((This "a1" <> This "a2") <> Both "a3" "b3") `shouldBe` This "a1" <> (This "a2" <> Both "a3" "b3")
      it "(This 'a1' <> This 'b2') <> This 'a3' `shouldBe` This 'a1' <> (This 'b2' <> This 'a3')" $ do
        thisOrThat ((This "a1" <> This "b2") <> This "a3") `shouldBe` This "a1" <> (This "b2" <> This "a3")
      it "(This 'a1' <> This 'b2') <> That 'b3' `shouldBe` This 'a1' <> (This 'b2' <> That 'b3')" $ do
        thisOrThat ((This "a1" <> This "b2") <> That "b3") `shouldBe` This "a1" <> (This "b2" <> That "b3")
      it "(This 'a1' <> This 'b2') <> Both 'a3' 'b3' `shouldBe` This 'a1' <> (This 'b2' <> Both 'a3' 'b3')" $ do
        thisOrThat ((This "a1" <> This "b2") <> Both "a3" "b3") `shouldBe` This "a1" <> (This "b2" <> Both "a3" "b3")
      it "(This 'a1' <> Both 'a2' 'b2') <> This 'a3' `shouldBe` This 'a1' <> (Both 'a2' 'b2' <> This 'a3')" $ do
        thisOrThat ((This "a1" <> Both "a2" "b2") <> This "a3") `shouldBe` This "a1" <> (Both "a2" "b2" <> This "a3")
      it "(This 'a1' <> Both 'a2' 'b2') <> That 'b3' `shouldBe` This 'a1' <> (Both 'a2' 'b2' <> That 'b3')" $ do
        thisOrThat ((This "a1" <> Both "a2" "b2") <> That "b3") `shouldBe` This "a1" <> (Both "a2" "b2" <> That "b3")
      it "(This 'a1' <> Both 'a2' 'b2') <> Both 'a3' 'b3' `shouldBe` This 'a1' <> (Both 'a2' 'b2' <> Both 'a3' 'b3')" $ do
        thisOrThat ((This "a1" <> Both "a2" "b2") <> Both "a3" "b3") `shouldBe` This "a1" <> (Both "a2" "b2" <> Both "a3" "b3")
      it "(That 'b1' <> This 'a2') <> This 'a3' `shouldBe` That 'b1' <> (This 'a2' <> This 'a3')" $ do
        thisOrThat ((That "b1" <> This "a2") <> This "a3") `shouldBe` That "b1" <> (This "a2" <> This "a3")
      it "(That 'b1' <> This 'a2') <> That 'b3' `shouldBe` That 'b1' <> (This 'a2' <> That 'b3')" $ do
        thisOrThat ((That "b1" <> This "a2") <> That "b3") `shouldBe` That "b1" <> (This "a2" <> That "b3")
      it "(That 'b1' <> This 'a2') <> Both 'a3' 'b3' `shouldBe` That 'b1' <> (This 'a2' <> Both 'a3' 'b3')" $ do
        thisOrThat ((That "b1" <> This "a2") <> Both "a3" "b3") `shouldBe` That "b1" <> (This "a2" <> Both "a3" "b3")
      it "(That 'b1' <> This 'b2') <> This 'a3' `shouldBe` That 'b1' <> (This 'b2' <> This 'a3')" $ do
        thisOrThat ((That "b1" <> This "b2") <> This "a3") `shouldBe` That "b1" <> (This "b2" <> This "a3")
      it "(That 'b1' <> This 'b2') <> That 'b3' `shouldBe` That 'b1' <> (This 'b2' <> That 'b3')" $ do
        thisOrThat ((That "b1" <> This "b2") <> That "b3") `shouldBe` That "b1" <> (This "b2" <> That "b3")
      it "(That 'b1' <> This 'b2') <> Both 'a3' 'b3' `shouldBe` That 'b1' <> (This 'b2' <> Both 'a3' 'b3')" $ do
        thisOrThat ((That "b1" <> This "b2") <> Both "a3" "b3") `shouldBe` That "b1" <> (This "b2" <> Both "a3" "b3")
      it "(That 'b1' <> Both 'a2' 'b2') <> This 'a3' `shouldBe` That 'b1' <> (Both 'a2' 'b2' <> This 'a3')" $ do
        thisOrThat ((That "b1" <> Both "a2" "b2") <> This "a3") `shouldBe` That "b1" <> (Both "a2" "b2" <> This "a3")
      it "(That 'b1' <> Both 'a2' 'b2') <> That 'b3' `shouldBe` That 'b1' <> (Both 'a2' 'b2' <> That 'b3')" $ do
        thisOrThat ((That "b1" <> Both "a2" "b2") <> That "b3") `shouldBe` That "b1" <> (Both "a2" "b2" <> That "b3")
      it "(That 'b1' <> Both 'a2' 'b2') <> Both 'a3' 'b3' `shouldBe` That 'b1' <> (Both 'a2' 'b2' <> Both 'a3' 'b3')" $ do
        thisOrThat ((That "b1" <> Both "a2" "b2") <> Both "a3" "b3") `shouldBe` That "b1" <> (Both "a2" "b2" <> Both "a3" "b3")
      it "(Both 'a1' 'b1' <> This 'a2') <> This 'a3' `shouldBe` Both 'a1' 'b1' <> (This 'a2' <> This 'a3')" $ do
        thisOrThat ((Both "a1" "b1" <> This "a2") <> This "a3") `shouldBe` Both "a1" "b1" <> (This "a2" <> This "a3")
      it "(Both 'a1' 'b1' <> This 'a2') <> That 'b3' `shouldBe` Both 'a1' 'b1' <> (This 'a2' <> That 'b3')" $ do
        thisOrThat ((Both "a1" "b1" <> This "a2") <> That "b3") `shouldBe` Both "a1" "b1" <> (This "a2" <> That "b3")
      it "(Both 'a1' 'b1' <> This 'a2') <> Both 'a3' 'b3' `shouldBe` Both 'a1' 'b1' <> (This 'a2' <> Both 'a3' 'b3')" $ do
        thisOrThat ((Both "a1" "b1" <> This "a2") <> Both "a3" "b3") `shouldBe` Both "a1" "b1" <> (This "a2" <> Both "a3" "b3")
      it "(Both 'a1' 'b1' <> This 'b2') <> This 'a3' `shouldBe` Both 'a1' 'b1' <> (This 'b2' <> This 'a3')" $ do
        thisOrThat ((Both "a1" "b1" <> This "b2") <> This "a3") `shouldBe` Both "a1" "b1" <> (This "b2" <> This "a3")
      it "(Both 'a1' 'b1' <> This 'b2') <> That 'b3' `shouldBe` Both 'a1' 'b1' <> (This 'b2' <> That 'b3')" $ do
        thisOrThat ((Both "a1" "b1" <> This "b2") <> That "b3") `shouldBe` Both "a1" "b1" <> (This "b2" <> That "b3")
      it "(Both 'a1' 'b1' <> This 'b2') <> Both 'a3' 'b3' `shouldBe` Both 'a1' 'b1' <> (This 'b2' <> Both 'a3' 'b3')" $ do
        thisOrThat ((Both "a1" "b1" <> This "b2") <> Both "a3" "b3") `shouldBe` Both "a1" "b1" <> (This "b2" <> Both "a3" "b3")
      it "(Both 'a1' 'b1' <> Both 'a2' 'b2') <> This 'a3' `shouldBe` Both 'a1' 'b1' <> (Both 'a2' 'b2' <> This 'a3')" $ do
        thisOrThat ((Both "a1" "b1" <> Both "a2" "b2") <> This "a3") `shouldBe` Both "a1" "b1" <> (Both "a2" "b2" <> This "a3")
      it "(Both 'a1' 'b1' <> Both 'a2' 'b2') <> That 'b3' `shouldBe` Both 'a1' 'b1' <> (Both 'a2' 'b2' <> That 'b3')" $ do
        thisOrThat ((Both "a1" "b1" <> Both "a2" "b2") <> That "b3") `shouldBe` Both "a1" "b1" <> (Both "a2" "b2" <> That "b3")
      it "(Both 'a1' 'b1' <> Both 'a2' 'b2') <> Both 'a3' 'b3' `shouldBe` Both 'a1' 'b1' <> (Both 'a2' 'b2' <> Both 'a3' 'b3')" $ do
        thisOrThat ((Both "a1" "b1" <> Both "a2" "b2") <> Both "a3" "b3") `shouldBe` Both "a1" "b1" <> (Both "a2" "b2" <> Both "a3" "b3")
  