{-# LANGUAGE InstanceSigs #-}

module Block4.Task2 where

-- | Structure representing a binary tree.
-- Constructors:
--   * Leaf has a value and no children
--   * Branch has only two children
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

-- | Functor instance for Tree.
instance Functor Tree where
  -- | Applies function to all values in leaves.
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap transform (Leaf value) = Leaf $ transform value
  fmap transform (Branch lhs rhs) =
    Branch (transform <$> lhs) (transform <$> rhs)

-- | Applicative instance for Tree.
instance Applicative Tree where
  -- | For two leaves applies function in the first one to value in another.
  -- For Leaf and Branch propagates application of a function to all leaves in this branch.
  -- For Branch and Leaf returns branch with functions in leaves applied to value in the leaf.
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf transform) (Leaf value) = Leaf $ transform value
  (<*>) (Leaf transform) (Branch lhs rhs) = 
    Branch (transform <$> lhs) (transform <$> rhs)
  (<*>) (Branch lhs rhs) leaf@(Leaf _) =  Branch (lhs <*> leaf) (rhs <*> leaf)
  (<*>) (Branch lhs1 rhs1) (Branch lhs2 rhs2) = 
    Branch (lhs1 <*> lhs2) (rhs1 <*> rhs2)

  -- | Pure for a value is Leaf with that value.
  pure :: a -> Tree a
  pure = Leaf

-- | Foldable instance for Tree
instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap transform (Leaf value) = transform value
  foldMap transform (Branch lhs rhs) = foldMap transform lhs <> foldMap transform rhs
  
-- | Traversable instance for Tree
instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse transform (Leaf value) = Leaf <$> (transform value)
  traverse transform (Branch lhs rhs) = 
    (Branch <$> (traverse transform lhs)) <*> (traverse transform rhs)
