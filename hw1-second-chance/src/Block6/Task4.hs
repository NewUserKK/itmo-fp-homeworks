module Block6.Task4
  ( listListParser
  ) where

import Control.Applicative
import Data.Char
import Block6.Task1
import Block6.Task2

-- | Parser of list of list of numbers separated by comma ignoring spaces.
-- String format: (<number of elements in list>, <exactly n elements>)*
-- Negative list length isn't allowed.
listListParser :: Parser Char [[Int]]
listListParser = ([] <$ (spaces *> eol)) <|> (listParser +:+ many (comma *> listParser) <* eol)

listParser :: Parser Char [Int]
listParser =
  Parser $ \s -> do
    (listSize, listRest) <- runParser numberParser s
    if read listSize == (0 :: Int)
      then return ([], listRest)
      else let listStartParser = (comma *> matchExactly (read listSize - 1) numberWithComma)
            in do (listStart, tailRest) <- runParser listStartParser listRest
                  (listTail, rest) <- runParser numberParser tailRest
                  return (map read $ listStart ++ [listTail], rest)

numberWithComma :: Parser Char String
numberWithComma = numberParser <* comma

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
