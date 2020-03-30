{-# LANGUAGE LambdaCase #-}

module Block6.Task3
 ( correctBracketSequence
 , numberParser
 ) where

import Block6.Task1
import Block6.Task2
import Control.Applicative
import Data.Char

correctBracketSequence :: Parser Char String
correctBracketSequence = eol <|> (correctBracketSequence' +++ eol)

correctBracketSequence' :: Parser Char String
correctBracketSequence' = rule +++ (eol <|> correctBracketSequence' <|> eps)
  where
    rule = lBracket +++ (correctBracketSequence' <|> eps) +++ rBracket

lBracket :: Parser Char String
lBracket = stream "("

rBracket :: Parser Char String
rBracket = stream ")"

-- number parser

numberParser :: Parser Char Int
numberParser = read <$> (plusOrMinus +++ some digit +++ eol)

plusOrMinus :: Parser Char String
plusOrMinus = (stream "+" *> eps) <|> stream "-" <|> eps

digit :: Parser Char Char
digit = satisfy isDigit
