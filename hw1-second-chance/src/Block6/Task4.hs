module Block6.Task4
  ( listListParser
  ) where

import Control.Applicative
import Data.Char
import Block6.Task1
import Block6.Task2

listListParser :: Parser Char [[Int]]
listListParser = ([[]] <$ eol) <|> (listParser +:+ many (comma *> listParser))

listParser :: Parser Char [Int]
listParser =
  Parser $ \s -> do
    (parseListSize, rest) <- runParser (numberParser <* comma) s
    (a, b) <- runParser (matchExactly (read parseListSize - 1) (numberParser <* comma)) rest
    (c, d) <- runParser numberParser b
    return (map read $ a ++ [c], d)

numberParser :: Parser Char String
numberParser = spaces *> (plusOrMinus +++ some digit) <* spaces

plusOrMinus :: Parser Char String
plusOrMinus = (stream "+" *> eps) <|> stream "-" <|> eps

digit :: Parser Char Char
digit = satisfy isDigit

spaces :: Parser Char String
spaces = many (element ' ')

comma :: Parser Char String
comma = stream ","
