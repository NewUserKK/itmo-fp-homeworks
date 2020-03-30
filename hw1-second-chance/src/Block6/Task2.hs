{-# LANGUAGE LambdaCase #-}

module Block6.Task2 where

import Data.Semigroup (Semigroup, (<>))
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
element c = satisfy (== c)

stream :: Eq s => [s] -> Parser s [s]
stream cs =
  Parser $ \case
    xs | prefix == cs -> Just (prefix, suffix)
      where
        (prefix, suffix) = splitAt (length cs) xs
    _ -> Nothing


-- Other combinators for future tasks 

eps :: Parser s [a]
eps = Parser $ \s -> Just ([], s)

eol :: Parser s [a]
eol = 
  Parser $ \case
    [] -> Just ([], [])
    _ -> Nothing 

(+++) :: Semigroup a => Parser s a -> Parser s a -> Parser s a
(+++) first second =
  Parser $ \s -> do
    (match, rest) <- runParser first s
    (sndMatch, sndRest) <- runParser second rest
    return (match <> sndMatch, sndRest)

(+:+) :: Parser s a -> Parser s [a] -> Parser s [a]
(+:+) first second =
  Parser $ \s -> do
    (match, rest) <- runParser first s
    (sndMatch, sndRest) <- runParser second rest
    return (match : sndMatch, sndRest)

matchExactly :: Int -> Parser s a -> Parser s [a]
matchExactly n parser
  | n > 0 = parser +:+ matchExactly (n - 1) parser
  | otherwise = eps