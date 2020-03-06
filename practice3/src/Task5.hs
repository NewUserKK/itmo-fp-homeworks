module Task5 where

import Data.Monoid

foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr binOp init = undefined