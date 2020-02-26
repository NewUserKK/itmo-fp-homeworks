module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement x = fix (x :)

fibonacci :: Integer -> Integer
fibonacci = fix fibonacci'
  where
    fibonacci' f n
      | n == 0 = 0
      | n == 1 = 1
      | otherwise = f (n - 1) + f (n - 2)

factorial :: Integer -> Integer
factorial = fix factorial'
  where
    factorial' f n
      | n == 0 = 1
      | otherwise = n * f (n - 1)

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix mapFix'
  where
    mapFix' _ _ [] = []
    mapFix' f transform (x:xs) = transform x : f transform xs
