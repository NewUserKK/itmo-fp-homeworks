module Block6.Task4 where

import Control.Applicative
import Block6.Task1
import Block6.Task2 as T2
import Block6.Task3

listListParser :: Parser Char [[Int]]
listListParser = eof <|> listParser +++ many (comma *> listParser)

listParser :: Parser Char [[Int]]
listParser =
  case elementsCount of
    Nothing -> empty
    Just (n, _) -> matchExactly n (numberParser <* comma)
  where
    elementsCount :: String -> Maybe (Int, String) 
    elementsCount s = runParser (numberParser <* comma) s

spaces :: Parser Char String
spaces = many (stream " ")

comma :: Parser Char String
comma = spaces *> stream "," <* spaces
