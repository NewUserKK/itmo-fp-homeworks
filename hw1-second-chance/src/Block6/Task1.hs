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
  fmap transform (Parser run) =
    Parser $ \stream -> do
      res <- run stream
      return $ Bifunctor.first transform res

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \x -> Just (a, x)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser runFunction) (Parser runValue) =
    Parser $ \stream -> do
      (transform, rest) <- runFunction stream
      valueRes <- runValue $ rest
      return $ Bifunctor.first transform valueRes

instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser run) bind =
    Parser $ \stream -> do
      (res, rest) <- run stream
      runParser (bind res) rest

instance Alternative (Parser s) where
   empty :: Parser s a
   empty = Parser $ const Nothing
   
   (<|>) :: Parser s a -> Parser s a -> Parser s a
   (<|>) (Parser runFirst) (Parser runSecond) = 
     Parser $ \stream -> runFirst stream <|> runSecond stream