{-# LANGUAGE InstanceSigs #-}

module Block4.Task3 where

-- | Structure representing non-empty list.
-- Has one constructor (:|) that takes first element on the left and
-- list of remaining elements on the right.
data NonEmpty a =
  a :| [a]
  deriving (Show)

-- | Functor instance for NonEmpty.
instance Functor NonEmpty where
  -- | Takes function and applies it to all elements in given list.
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap transform (x :| xs) = (transform x) :| (transform <$> xs)

-- Applicative instance for NonEmpty.
instance Applicative NonEmpty where
  -- | Pure for a value is non-empty list with that value.
  pure :: a -> NonEmpty a
  pure = (:| [])
  
  -- | Take list of functions, list of elements and apply each function
  -- to each element collecting results in one non-empty list.
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| fs) (x :| xs) = (f x) :| ((f <$> xs) ++ (fs <*> [x]) ++ (fs <*> xs))

-- | Monad instance for NonEmpty.
instance Monad NonEmpty where
  -- | Take list of elements, transform function and return concatenation
  -- of all results of application of the function to each element.
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| xs) f = y :| (ys ++ (xs >>= toList . f))
    where
      (y :| ys) = f x
      toList :: NonEmpty a -> [a]
      toList (v :| vs) = v : vs

-- | Foldable instance for NonEmpty.
instance Foldable NonEmpty where
  foldMap :: (Monoid m) => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = f x <> foldMap f xs

-- | Traversable instance for NonEmpty.
instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = ((:|) <$> (f x)) <*> traverse f xs
