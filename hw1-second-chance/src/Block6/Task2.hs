{-# LANGUAGE LambdaCase #-}

module Block6.Task2 where

import Data.Semigroup (Semigroup, (<>))
import Block6.Task1

-- | Parser that always succeeds
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | Parser that indicates end of input
eof :: Parser s ()
eof =
  Parser $ \case
    [] -> Just ((), [])
    _ -> Nothing

-- | Parse first element of a stream if it satisfies given predicate 
satisfy :: (s -> Bool) -> Parser s s
satisfy predicate =
  Parser $ \case
    (x : xs) | predicate x -> Just (x, xs)
    _ -> Nothing

-- | Parse first element of a stream if it is equal to given element
element :: Eq s => s -> Parser s s
element c = satisfy (== c)

-- | Parse first part of a stream if it is equal to given part of a stream
stream :: Eq s => [s] -> Parser s [s]
stream cs =
  Parser $ \case
    xs | prefix == cs -> Just (prefix, suffix)
      where
        (prefix, suffix) = splitAt (length cs) xs
    _ -> Nothing


-- Other combinators for future tasks 

-- | Overload of [ok] for a list.
eps :: Parser s [a]
eps = [] <$ ok

-- | Overload of [eof] for a list.
eol :: Parser s [a]
eol = [] <$ eof

-- | Sequential application of two parsers with the result as [mconcat] of parsing results.
-- e.g. for String values will return their concatenation as a result.
(+++) :: Semigroup a => Parser s a -> Parser s a -> Parser s a
(+++) first second = (<>) <$> first <*> second

-- | Sequential application of two parsers with the result as list of parsing results.
-- e.g. for lists will return list lists.
(+:+) :: Parser s a -> Parser s [a] -> Parser s [a]
(+:+) first second = (:) <$> first <*> second

-- | Parse exactly [n] tokens from a stream and return list of parsed tokens.
matchExactly :: Int -> Parser s a -> Parser s [a]
matchExactly n parser
  | n > 0 = parser +:+ matchExactly (n - 1) parser
  | otherwise = eps