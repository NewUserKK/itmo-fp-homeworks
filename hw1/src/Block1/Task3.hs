{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Block1.Task3
  ( Tree(..)
  , isEmpty
  , size
  , find
  , Block1.Task3.insert
  , remove
  , Block1.Task3.fromList
  ) where

import Data.List.NonEmpty as NE
import Data.Monoid ((<>))

-- | Structure representing binary tree.
-- Constructors:
--   * Leaf has no children and no associated data.
--   * Node has non-empty list of equal elements and left and right children.
data Tree a
  = Leaf
  | Node
      { nodeValues :: NonEmpty a
      , nodeLeft   :: Tree a
      , nodeRight  :: Tree a
      }
  deriving (Show, Eq)

-- | Class representing binary search tree.
-- Binary search tree is the tree that satisfy following rule:
-- for every node: all values of a left child of a node is less than node value,
-- and all values of a right child is greater than node value.
--
-- [t] is type of tree and [a] is type of elements of that tree
-- so implementors are supposed to look like (t a).
class Ord a => BinarySearchTree a t | t -> a where
  -- | Check if tree is empty
  isEmpty :: t -> Bool

  -- | Return number of nodes in the tree.
  -- (Note: not the total number of elements in this nodes)
  size :: t -> Int

  -- | Check if element is present in the tree.
  -- Returns a list of all elements associated with the found node if any,
  -- and Nothing otherwise.
  find :: a -> t -> Maybe (NonEmpty a)

  -- | Insert element in the tree preserving class invariant.
  insert :: a -> t -> t

  -- | Remove element from the tree preserving class invariant.
  remove :: a -> t -> t

  -- | Construct tree from a list.
  fromList :: [a] -> t

-- | Instance of BST for a Tree.
-- Tree is not balanced in any way.
instance Ord a => BinarySearchTree a (Tree a) where
  isEmpty :: Tree a -> Bool
  isEmpty Leaf = True
  isEmpty _    = False

  size :: Tree a -> Int
  size Leaf = 0
  size (Node _ left right) = 1 + size left + size right

  find :: a -> Tree a -> Maybe (NonEmpty a)
  find _ Leaf = Nothing
  find x (Node values left right)
    | x == v = Just values
    | x < v = find x left
    | otherwise = find x right
    where
      v = NE.head values

  insert :: a -> Tree a -> Tree a
  insert x Leaf = Node (x :| []) Leaf Leaf
  insert x node@(Node values left right)
    | x == v = node { nodeValues = x <| values }
    | x < v = node { nodeLeft = Block1.Task3.insert x left }
    | otherwise = node { nodeRight = Block1.Task3.insert x right }
    where
      v = NE.head values

  remove :: a -> Tree a -> Tree a
  remove _ Leaf = Leaf
  remove x node@(Node values left right)
    | x == v =
      case node of
        Leaf -> Leaf     -- for the sake of compiler to not throw a warning
        Node _ Leaf Leaf -> Leaf
        Node _ nLeft Leaf -> nLeft
        Node _ Leaf nRight -> nRight
        Node _ (Node _ _ Leaf) nRight -> left { nodeRight = nRight }
        Node _ nLeft nRight -> left { nodeRight = updateDeepestRightNode nLeft nRight }
    | x < v = node { nodeLeft = Block1.Task3.remove x left }
    | otherwise = node { nodeRight = Block1.Task3.remove x right }
    where
      v = NE.head values

      updateDeepestRightNode :: Tree a -> Tree a -> Tree a
      updateDeepestRightNode Leaf _ = Leaf
      updateDeepestRightNode currentNode@(Node _ _ Leaf) newRight =
        currentNode { nodeRight = newRight }
      updateDeepestRightNode (Node _ _ currentRight) newRight =
        updateDeepestRightNode currentRight newRight

  fromList :: [a] -> Tree a
  fromList = foldr Block1.Task3.insert Leaf


-- Block2.Task1 --

-- | Foldable instance for a Tree
-- Folding is happening from right-deepest node to the left-deepest
-- in following order for every node: right child -> node value -> left child.
instance Foldable (Tree) where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ initial Leaf = initial
  foldr op initial (Node values left right) =
    foldr op (foldr op (foldr op initial right) values) left

  foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap transform (Node values left right) =
    foldMap transform left <> foldMap transform values <> foldMap transform right
