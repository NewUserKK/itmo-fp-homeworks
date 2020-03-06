{-# LANGUAGE InstanceSigs #-}

module Task4 where

import Prelude hiding (Foldable, foldMap)
import Data.Monoid ((<>))

class Foldable f where
    foldMap :: Monoid m => (a -> m) -> f a -> m

instance Foldable [] where
    foldMap :: Monoid m => (a -> m) -> [a] -> m
    foldMap transform (x:xs) = transform x <> foldMap transform xs 
    foldMap _ [] = mempty
