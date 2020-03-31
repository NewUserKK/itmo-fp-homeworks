{-# LANGUAGE LambdaCase #-}

module Block6.Task3
 ( correctBracketSequence
 , numberParser
 ) where

import Block6.Task1
import Block6.Task2
import Control.Applicative
import Data.Char

-- | Parse correct bracket sequence.
-- Correct bracket sequence is a string that satisfies following grammar:
--   S -> (S)
--   S -> S S
--   S -> eps
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


-- | Parse number.
-- Number can be parsed from a string that satisfies following regexp: (+-)?(\d)+
numberParser :: Parser Char Int
numberParser = read <$> (plusOrMinus +++ some digit +++ eol)

plusOrMinus :: Parser Char String
plusOrMinus = (stream "+" *> eps) <|> stream "-" <|> eps

digit :: Parser Char Char
digit = satisfy isDigit
