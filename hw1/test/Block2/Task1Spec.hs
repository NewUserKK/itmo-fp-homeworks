module Block2.Task1Spec where

import Data.Foldable
import Data.List
import Test.Hspec
import Test.QuickCheck
import Block1.Task3

fromListTyped :: [Integer] -> Tree Integer
fromListTyped = fromList

spec :: Spec
spec = do
  describe "Block2.Task1.Foldable" $ do
    it "toList . fromList should act as sort" $ do
      property $ \l -> (toList . fromListTyped) l == sort l
