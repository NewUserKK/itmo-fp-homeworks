{-# LANGUAGE InstanceSigs #-}

module Task7 where

data Tree a = Leaf (Maybe a)
    | Branch (Tree a) (Maybe a) (Tree a)
    deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap transform (Leaf value) = Leaf (fmap transform value)
  fmap transform (Branch lhs value rhs) =
    Branch (fmap transform lhs) (fmap transform value) (fmap transform rhs)

instance Applicative Tree where
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf transform) (Leaf value) = Leaf $ transform <*> value
  (<*>) (Leaf transform) (Branch _ value _) = Leaf $ transform <*> value
  (<*>) (Branch lhs transform rhs) (Leaf value) = Leaf $ transform <*> value
  (<*>) (Branch lhs1 transform rhs1) (Branch lhs2 value rhs2) =
     Branch (lhs1 <*> lhs2) (transform <*> value) (rhs1 <*> rhs2)

  pure :: a -> Tree a
  pure a = Leaf (Just a)
