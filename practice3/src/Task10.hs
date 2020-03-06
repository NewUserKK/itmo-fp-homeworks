module Task10 where

import Prelude hiding (traverse)

traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
traverse transform xs = sequenceA $ fmap transform xs

-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
