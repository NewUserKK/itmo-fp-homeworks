{-# LANGUAGE InstanceSigs #-}

module Task9 where

import           Data.Monoid ((<>))

data Tree a = Leaf a
    | Branch (Tree a) (Tree a)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap transform (Leaf value) = Leaf (transform value)
  fmap transform (Branch lhs rhs) =
    Branch (transform <$> lhs) (transform <$> rhs)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap transform (Leaf a) = transform a
  foldMap transform (Branch lhs rhs) =
    (foldMap transform lhs) <> (foldMap transform rhs)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse transform (Leaf value) = Leaf <$> (transform value)
  traverse transform (Branch lhs rhs) =
    (Branch <$> (traverse transform lhs)) <*> (traverse transform rhs)
