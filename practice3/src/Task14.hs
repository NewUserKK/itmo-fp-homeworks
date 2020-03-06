{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Task14 where

import           Data.Semigroup
import           Task12

class ConcatablePath a b where
  (</>) :: a -> b -> a

instance ConcatablePath (Path Rel) (Path Rel) where
  (</>) :: Path Rel -> Path Rel -> Path Rel
  (</>) (Path lhs) (Path rhs) = Path $ lhs <> rhs

instance ConcatablePath (Path Abs) (Path Rel) where
  (</>) :: Path Abs -> Path Rel -> Path Abs
  (</>) (Path lhs) (Path rhs) = Path $ lhs <> rhs
