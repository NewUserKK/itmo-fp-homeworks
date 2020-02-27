module Main where

-------------------------
-- Practice 1
-------------------------


------------------------------------------
-- Task 1
--
-- Implement task 2 from block 3 (castles)
-- of homework located at
-- https://hackmd.io/@mtb49Ag9TOmTeG0qf_6Fwg/BkcpMch44
--
------------------------------------------


--------------------------------------------
-- Task 2
--
-- Implement `Ord` instance for newtype
-- below.
--
-- It should follow the described semantics:
--
--  1) If both strings being compared start
--  from a digit symbol (`0..9`), read
--  numeric prefixes and compare strings by
--  these prefixes. If both strings start
--  from the same number (e.g. `01aba` and
--  `1caba`), comparison is performed by
--  rest of string characters
--  case-insensitive
--  2) Otherwise, compare two strings
--  case-insensitive
--------------------------------------------

newtype FName = FName String


--------------------------------------------
-- Task 3
--
-- When launched from ghci, following
-- results will be printed.
-- Explain the difference in calls
-- (why one call returns while other
-- goes into infinite loop):
--
-- > sumAndLogD [8] (BoxD 2)
-- Just 3.0
--
-- > sumAndLog [8] (Box 2)
-- Just 3.0
--
-- > sumAndLog [8 -10] loop
-- Nothing
--
-- > sumAndLogD [8 -10] loop
-- {.. infitinte loop ..}
--
--------------------------------------------

newtype Box a = Box a
data BoxD a = BoxD a

sumAndLog as (Box base) = let s = sum as in if s < 0 then Nothing else Just (log s / log base)
sumAndLogD as (BoxD base) = let s = sum as in if s < 0 then Nothing else Just (log s / log base)

loop = loop


main = pure ()
