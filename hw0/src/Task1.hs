{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left a) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

associator :: (a, (b, c)) -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

type (<->) a b = (a -> b, b -> a)

eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc =
  ( \case
      (Left a) -> Left (Left a)
      (Right (Left b)) -> Left (Right b)
      (Right (Right c)) -> Right c
  , \case
      (Left (Left a)) -> Left a
      (Left (Right b)) -> Right (Left b)
      (Right c) -> Right (Right c)
  )
