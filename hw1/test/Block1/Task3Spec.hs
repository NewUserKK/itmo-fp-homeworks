module Block1.Task3Spec where

import Test.Hspec
import Data.List.NonEmpty as NE
import Block1.Task3 as T3

emptyTree :: Tree Int
emptyTree = Leaf

tree :: Tree Int
tree = Node
  { nodeValues = 10 :| []
  , nodeLeft = Node
    { nodeValues = 8 :| [8]
    , nodeLeft = Node
      { nodeValues = 5 :| []
      , nodeLeft = Leaf
      , nodeRight = Leaf
      }
    , nodeRight = Leaf
    }
  , nodeRight = Node
    { nodeValues = 13 :| []
    , nodeLeft = Node
      { nodeValues = 11 :| []
      , nodeLeft = Leaf
      , nodeRight = Leaf
      }
    , nodeRight = Node
      { nodeValues = 15 :| []
      , nodeLeft = Leaf
      , nodeRight = Node
        { nodeValues = 16 :| []
        , nodeLeft = Leaf
        , nodeRight = Leaf
        }
      }
    }
  }

spec :: Spec
spec = do
  describe "Block1.Task3.isEmpty" $ do
    it "Leaf is empty" $ do
      isEmpty emptyTree `shouldBe` True
    it "Node is not empty" $ do
      isEmpty tree `shouldBe` False
  describe "Block1.Task3.size" $ do
    it "size of an empty tree is zero" $ do
      size emptyTree `shouldBe` 0
    it "size calculates correctly" $ do
      size tree `shouldBe` 7
  describe "Block1.Task3.find" $ do
    it "should return associated value list" $ do
      find (8 :: Int) tree `shouldBe` Just ((8 :: Int) :| [8])
    it "should return Nothing if value is not found" $ do
      find (20 :: Int) tree `shouldBe` Nothing
  describe "Block1.Task3.insert" $ do
    it "should insert correctly" $ do
      let newTree = tree {
        nodeLeft = (nodeLeft tree) {
          nodeRight = Node
            { nodeValues = 9 :| []
            , nodeLeft = Leaf
            , nodeRight = Leaf }
          }
        }
       in
        T3.insert (9 :: Int) tree `shouldBe` newTree
  describe "Block1.Task3.remove" $ do
    it "should remove correctly" $ do
      let newTree = tree {
        nodeRight = (nodeLeft $ nodeRight tree) {
          nodeRight = nodeRight $ nodeRight tree
          }
        }
       in
        T3.remove (13 :: Int) tree `shouldBe` newTree
    it "should not change the tree if removed not existing element" $ do
      T3.remove (1 :: Int) tree `shouldBe` tree
  describe "Block1.Task3.fromList" $ do
    it "constructs empty tree from empty list" $ do
      T3.fromList ([] :: [Int]) `shouldBe` Leaf
    it "constructs correct tree from list" $ do
      T3.fromList ([16, 15, 11, 5, 13, 8, 8, 10] :: [Int]) `shouldBe` tree
