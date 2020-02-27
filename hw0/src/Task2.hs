{-# LANGUAGE TypeOperators #-}

module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void()

type Neg a = a -> Int

doubleNeg :: a -> Neg (Neg a)
-- a -> (a -> Void) -> Void
doubleNeg a f = f a

excludedNeg :: Neg (Neg (Either a (Neg a)))
-- (Either a (a -> Void) -> Void) -> Void
excludedNeg f = f (Right (\x -> f (Left x)))

-- not provable in intuitionistic logic therefore not implementable
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- not provable in intuitionistic logic therefore not implementable
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
-- (((a -> Void) -> Void) -> Void) -> (a -> Void)
thirdNegElim f a = f $ doubleNeg a
