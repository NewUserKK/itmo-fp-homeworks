{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Task1 where

import           Data.Semigroup (Semigroup, (<>))

newtype Last a = Last (Maybe a)

instance Semigroup (Last a) where
  (<>) :: Last a -> Last a -> Last a
  (<>) lhs (Last Nothing) = lhs
  (<>) _ rhs = rhs

instance Monoid (Last a) where
  mempty :: Last a
  mempty = Last Nothing