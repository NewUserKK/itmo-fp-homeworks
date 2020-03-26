module Task8 where

(<$) :: Functor f => a -> f b -> f a
(<$) value functor = (const value) <$> functor

(*>) :: Applicative f => f a -> f b -> f b
(*>) lhs rhs = id Task8.<$ lhs <*> rhs
