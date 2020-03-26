{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  ( Nat(..)
  , Natural
  , fromInteger
  , Block1.Task2.toInteger
  , Block1.Task2.isEven
  ) where

-- | Structure representing natural numbers.
-- Has two constructors:
--   * Z is for zero
--   * S Nat is for incrementing given Nat
data Nat 
    = Z
    | S Nat
    deriving Show

-- | Class for natural numbers
class (Ord a, Num a) => Natural a where
  -- | Convert natural number to Integer
  toInteger :: a -> Integer
  
  -- | Check if natural number is even
  isEven :: a -> Bool

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) n m = n <= m && m <= n

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=) Z Z = True
  (<=) (S _) Z = False
  (<=) Z (S _) = True
  (<=) (S n) (S m) = n <= m

-- | Num instance for Nat.
-- Due to implementation, does not have additive inverse of negate.
instance Num Nat where
  -- | Add two natural numbers.
  (+) :: Nat -> Nat -> Nat
  (+) n Z = n
  (+) Z m = m
  (+) (S n) (S m) = S $ S $ n + m

  -- | Subtract two natural numbers. If left less than right then return Z 
  (-) :: Nat -> Nat -> Nat
  (-) n Z = n
  (-) Z _ = Z
  (-) (S n) (S m) = n - m

  -- | Multiply two natural numbers.
  (*) :: Nat -> Nat -> Nat
  (*) Z _ = Z
  (*) (S n) m = m + (n * m)

  -- | Return number itself as there is no point in negating natural numbers
  negate :: Nat -> Nat
  negate = id

  -- | Return absolute value of a natural number.
  -- This implementation returns itself as natural numbers are always not negative
  abs :: Nat -> Nat
  abs = id
  
  -- | Return signum of a natural number.
  -- Will return Z for Z and S Z otherwise.
  signum :: Nat -> Nat
  signum Z = Z
  signum (S _) = S Z

  -- | Construct Nat from Integer.
  -- Throws error if given Integer is negative.
  fromInteger :: Integer -> Nat
  fromInteger n
    | n == 0 = Z
    | n > 0 = S $ fromInteger (n - 1)
    | otherwise = error "Cannot construct Nat from negative number"

instance Natural Nat where
  toInteger :: Nat -> Integer
  toInteger Z = 0
  toInteger (S n) = (Block1.Task2.toInteger n) + 1

  isEven :: Nat -> Bool
  isEven Z = True
  isEven (S n) = not $ isEven n
