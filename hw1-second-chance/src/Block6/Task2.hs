{-# LANGUAGE LambdaCase #-}

module Block6.Task2 where

import Data.Monoid ((<>))
import Block6.Task1

ok :: Parser s [a]
ok = Parser $ \s -> Just ([], s)

eof :: Parser s [a]
eof =
  Parser $ \case
    [] -> Just ([], [])
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

(+++) :: Monoid a => Parser s a -> Parser s a -> Parser s a
(+++) first second =
  Parser $ \s -> do
    (match, rest) <- runParser first s
    (sndMatch, sndRest) <- runParser second rest
    return (match <> sndMatch, sndRest)
    
notParser :: Monoid a => Parser s a -> Parser s a
notParser parser = 
  Parser $ \s -> 
    case runParser parser s of
       Nothing -> Just (mempty, s)
       _ -> Nothing
       
some :: Monoid a => Parser s a -> Parser s a
some parser = 
  Parser $ \s -> do
    (firstMatch, firstRest) <- runParser parser s
    (match, rest) <- 
      case runParser (some parser) firstRest of
        Nothing -> Just (mempty, firstRest)
        x -> x
    return (firstMatch <> match, rest)