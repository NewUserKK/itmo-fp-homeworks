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

(+++) :: Semigroup a => Parser s a -> Parser s a -> Parser s a
(+++) first second =
  Parser $ \s -> do
    (match, rest) <- runParser first s
    (sndMatch, sndRest) <- runParser second rest
    return (match <> sndMatch, sndRest)

eps :: Parser s [a]
eps = Parser $ \s -> Just ([], s)

eol :: Parser s [a]
eol = 
  Parser $ \case
    [] -> Just ([], [])
    _ -> Nothing 

--notParser :: Monoid a => Parser s a -> Parser s a
--notParser parser = 
--  Parser $ \s -> 
--    case runParser parser s of
--       Nothing -> Just (mempty, s)
--       _ -> Nothing

--some :: Monoid a => Parser s a -> Parser s a
--some parser =
--  Parser $ \s -> do
--    (firstMatch, firstRest) <- runParser parser s
--    (match, rest) <-
--      case runParser (some parser) firstRest of
--        Nothing -> Just (mempty, firstRest)
--        x -> x
--    return (firstMatch <> match, rest)
--
--many :: Monoid a => Parser s a -> Parser s a
--many parser =
--  Parser $ \s ->
--    case runParser parser s of
--      Nothing -> Just (mempty, s)
--      Just (firstMatch, firstRest) -> do
--        (match, rest) <-
--          case runParser (many parser) firstRest of
--            Nothing -> Just (mempty, firstRest)
--            x -> x
--        return (firstMatch <> match, rest)

matchExactly :: Int -> Parser s [a] -> Parser s [a]
matchExactly 0 _ = eps
matchExactly n parser =
  Parser $ \s -> runParser (parser +++ matchExactly (n - 1) parser) s
