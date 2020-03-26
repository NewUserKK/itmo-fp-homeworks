{-# LANGUAGE InstanceSigs #-}

module Block3.Task2 where

-- | Structure representing non-empty list.
-- Has one constructor (:|) that takes first element on the left and
-- list of remaining elements on the right.
data NonEmpty a =
  a :| [a]
  deriving (Show, Eq)

-- | Semigroup instance for NonEmpry.
-- Returns concatenation of given lists.
instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ (y : ys))

-- | Structure representing either one value or another or both at the same time.
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show, Eq)

-- | Semigroup instance for ThisOrThat.
-- Constructs [Both] from [This] and [That]
-- or updates [Both] with [This] or [That] value 
-- or takes left element otherwise.
instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) (This a) (Both _ b) = Both a b
  (<>) (That b) (Both a _) = Both a b
  (<>) (This a) (That b) = Both a b
  (<>) (That b) (This a) = Both a b
  (<>) a _ = a

