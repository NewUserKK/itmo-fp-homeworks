{-# LANGUAGE LambdaCase #-}

module Block6.Task2 where

import Block6.Task1

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof =
  Parser $ \case
    [] -> Just ((), [])
    _ -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy predicate =
  Parser $ \case
    (x : xs) | predicate x -> Just (x, xs)
    _ -> Nothing

element :: Eq s => s -> Parser s s
element c =
  Parser $ \case
    (x : xs) | x == c -> Just (c, xs)
    _ -> Nothing

stream :: Eq s => [s] -> Parser s [s]
stream cs =
  Parser $ \case
    xs | prefix == cs -> Just (prefix, suffix)
      where
        (prefix, suffix) = splitAt (length cs) xs
    _ -> Nothing
