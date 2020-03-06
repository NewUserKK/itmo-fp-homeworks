module Task3 where

import Data.Monoid ()

foldMap :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
foldMap transform = foldr ((<>) . transform) mempty
