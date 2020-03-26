module Block3.Task1 where

import Data.Maybe
import Data.Semigroup ((<>))

-- | Take list of Maybes with lists and return concatenation of all inner lists.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat values = fromJust $ (foldr (<>) Nothing values) <> (Just [])