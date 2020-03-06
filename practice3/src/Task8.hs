module Task8 where

(<$) :: Functor f => a -> f b -> f a
(<$) value functor = (const value) <$> functor

-- TODO: what?
(*>) :: Applicative f => f a -> f b -> f b
(*>) _ rhs = rhs
