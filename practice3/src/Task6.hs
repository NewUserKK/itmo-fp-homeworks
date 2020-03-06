{-# LANGUAGE InstanceSigs #-}

module Task6
  ( Point3D(..)
  ) where

data Point3D a = Point3D a a a
    deriving (Show, Eq)

instance Functor Point3D where
  fmap :: (a -> b) -> Point3D a -> Point3D b
  fmap transform point = (Point3D transform transform transform) <*> point

instance Applicative Point3D where
  (<*>) :: Point3D (a -> b) -> Point3D a -> Point3D b
  (<*>) (Point3D f1 f2 f3) (Point3D x y z) = Point3D (f1 x) (f2 y) (f3 z)

  pure :: a -> Point3D a
  pure a = Point3D a a a