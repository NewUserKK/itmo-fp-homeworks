module Task3
  ( composition
  , identity
  , contraction
  , permutation
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k = const

composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (k s) k

identity :: a -> a
identity = s k k

contraction :: (a -> a -> b) -> a -> b
contraction = s s (k (s k k))

permutation :: (a -> b -> c) -> b -> a -> c
permutation = s ((s (k s) k) (s (k s) k) s) (k k)
