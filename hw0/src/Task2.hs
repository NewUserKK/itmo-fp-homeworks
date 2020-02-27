{-# LANGUAGE TypeOperators #-}

module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void ()

type Neg a = a -> Int

doubleNeg :: a -> Neg (Neg a)
doubleNeg a f = f a

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg f = f (Right (\x -> f (Left x)))

-- not provable in intuitionistic logic therefore not implementable
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- not provable in intuitionistic logic therefore not implementable
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f a = f $ doubleNeg a
