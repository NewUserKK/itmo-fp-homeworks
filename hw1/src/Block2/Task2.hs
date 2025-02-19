{-# LANGUAGE ScopedTypeVariables #-}

module Block2.Task2 where

import Data.List.NonEmpty

-- | Split list of values by given value.
-- Return non-empty list of lists of stored parts.
splitOn :: forall a . Eq a => a -> [a] -> NonEmpty [a]
splitOn splitElement list =
  case splitResult of
    Just result -> result
    Nothing -> [] :| []
  where
    splitResult :: Maybe (NonEmpty [a])
    splitResult = nonEmpty $ (buffer : splitted)
    
    (splitted, buffer) = foldr foldFunc ([], []) list
    
    foldFunc :: a -> ([[a]], [a]) -> ([[a]], [a])
    foldFunc x acc
      | x == splitElement = ((snd acc : fst acc), [])
      | otherwise = (fst acc, x : snd acc)
