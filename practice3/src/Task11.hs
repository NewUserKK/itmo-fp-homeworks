module Task11 where

import Prelude hiding (sequenceA)

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequenceA = traverse id