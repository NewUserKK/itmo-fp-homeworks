{-# LANGUAGE InstanceSigs #-}

module Block6.Task1 where

import Control.Applicative
import qualified Data.Bifunctor as Bifunctor

-- | Structure representing parser.
-- Essentially is just a function that takes stream and return
-- optional result of a parsing as pair of parsed token and remaining stream.
newtype Parser s a =
  Parser
    { runParser :: [s] -> Maybe (a, [s])
    }

-- | Functor instance for parser
instance Functor (Parser s) where
  -- | Returns parser that applies given function to the result of parsing
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap transform parser =
    Parser $ \stream -> do
      match <- runParser parser stream
      return $ Bifunctor.first transform match

-- | Applicative instance for parser
instance Applicative (Parser s) where
  -- | Returns parser that always return passed value without consuming any input
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)

  -- | Takes parser that parses function, parser that parses value
  -- and return parser that applies parsed function to parsed value
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) functionParser valueParser =
    Parser $ \stream -> do
      (transform, rest) <- runParser functionParser stream
      match <- runParser valueParser rest
      return $ Bifunctor.first transform match

-- | Monad instance for parser
instance Monad (Parser s) where
  -- | Returns parser that runs parser produced from the result of the first parsing
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) parser transform =
    Parser $ \stream -> do
      (match, rest) <- runParser parser stream
      runParser (transform match) rest

-- | Alternative instance for parser
instance Alternative (Parser s) where
  -- | Returns parser that always fails
  empty :: Parser s a
  empty = Parser $ const Nothing
  
  -- | Applies leftmost non-failing parser
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) first second = 
    Parser $ \stream -> runParser first stream <|> runParser second stream
