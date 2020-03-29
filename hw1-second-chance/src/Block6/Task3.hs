{-# LANGUAGE LambdaCase #-}

module Block6.Task3 where

import Block6.Task1
import Block6.Task2 as T2
import Control.Applicative
import Data.Char

numberParser :: Parser Char Int
numberParser =
  Parser $ \s -> do
    (match, rest) <- runParser (plusOrMinus +++ T2.some digit +++ eof) s
    return (read match :: Int, rest)

plusOrMinus :: Parser Char String
plusOrMinus = (stream "+" *> ok) <|> stream "-" <|> ok

digit :: Parser Char String
digit =
  Parser $ \case
    (x : xs) | isDigit x -> Just ([x], xs)
    _ -> Nothing
