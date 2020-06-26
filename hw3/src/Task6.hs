{-# LANGUAGE Rank2Types #-}

module Task6 where

import Task5.FS
import Lens.Micro

cd :: String -> Traversal' FS FS
cd cdName = _Dir . contents . traversed . (filtered (\dir -> dir ^. _Dir . name == cdName))

ls :: Traversal' FS String
ls = _Dir . contents . traversed . name

file :: String -> Traversal' FS String
file fileName = _Dir . contents . traversed . (filtered (\f -> f ^. _File . name == fileName)) . name