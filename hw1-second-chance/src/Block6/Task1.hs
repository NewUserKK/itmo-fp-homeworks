{-# LANGUAGE InstanceSigs #-}

module Block6.Task1 where

import Control.Applicative
import qualified Data.Bifunctor as Bifunctor

data Parser s a =
  Parser
    { runParser :: [s] -> Maybe (a, [s])
    }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap transform parser =
    Parser $ \stream -> do
      match <- runParser parser stream
      return $ Bifunctor.first transform match

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \x -> Just (a, x)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) functionParser valueParser =
    Parser $ \stream -> do
      (transform, rest) <- runParser functionParser stream
      match <- runParser valueParser rest
      return $ Bifunctor.first transform match

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) parser bind =
    Parser $ \stream -> do
      (match, rest) <- runParser parser stream
      runParser (bind match) rest

instance Alternative (Parser s) where
   empty :: Parser s a
   empty = Parser $ const Nothing
   
   (<|>) :: Parser s a -> Parser s a -> Parser s a
   (<|>) (Parser runFirst) (Parser runSecond) = 
     Parser $ \stream -> runFirst stream <|> runSecond stream