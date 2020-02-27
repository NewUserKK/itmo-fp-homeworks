module Task5
  ( Nat
  , zero
  , succChurch
  , churchPlus
  , churchMult
  , churchToInt
  ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f x = n f (m f x)

churchMult :: Nat a -> Nat a -> Nat a
churchMult n m f x = n (m f) x

churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0
