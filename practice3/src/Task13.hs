{-# LANGUAGE InstanceSigs #-}

module Task13 where

import           Data.List
import           Data.Monoid    ()
import           Data.Semigroup ()
import           Task12

instance Semigroup (Path a) where
  (<>) :: Path a -> Path a -> Path a
  (<>) (Path lhs) (Path rhs) = Path $ lhs <> rhs

instance Monoid (Path a) where
  mempty :: Path a
  mempty = Path []

isSubPath :: Path a -> Path b -> Bool
isSubPath (Path lhs) (Path rhs) = lhs `isInfixOf` rhs
