{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char

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
data City = City
    { cityHouses          :: Houses
    , cityCastle          :: Maybe Castle
    , citySpecialBuilding :: Maybe (Either Church Library)
    }

data Houses = Houses
    { housesHouse :: House
    , housesRest  :: [House]
    }

data House = House
    { houseFamily :: Family
    }

data Family = Family
    { familyCitizen1 :: Citizen
    , familyCitizen2 :: Maybe Citizen
    , familyCitizen3 :: Maybe Citizen
    , familyCitizen4 :: Maybe Citizen
    }

data Citizen = Citizen

data Castle = Castle
    { castleWalls :: Maybe Walls
    , castleLord  :: Maybe Lord
    }

data Lord = Lord

data Church = Church

data Library = Library

data Walls = Walls

data LordPopulationError = MissingCastleError
    | LordExistError

data WallsBuildingError = NotEnoughPeopleError
    | NoLordError
    | NoCastleError

buildCastle :: City -> Either Bool City
buildCastle city =
  case cityCastle city of
    Just _  -> Left False
    Nothing -> Right city {cityCastle = Just $ Castle Nothing Nothing}

buildLibrary :: City -> Either Bool City
buildLibrary city =
  case citySpecialBuilding city of
    Just _  -> Left False
    Nothing -> Right city {citySpecialBuilding = Just $ Right Library}

buildChurch :: City -> Either Bool City
buildChurch city =
  case citySpecialBuilding city of
    Just _  -> Left False
    Nothing -> Right city {citySpecialBuilding = Just $ Left Church}

buildHouse :: City -> Family -> City
buildHouse city family = city {cityHouses = newHouses}
  where
    newHouses = oldHouses {housesRest = restHouses ++ [House family]}
    oldHouses = cityHouses city
    restHouses = housesRest oldHouses

hasLord :: City -> Bool
hasLord city =
  case cityCastle city of
    Just castle ->
      case castleLord castle of
        Just _  -> True
        Nothing -> False
    Nothing -> False

populateCityWithLord :: City -> Lord -> Either LordPopulationError City
populateCityWithLord city lord =
  case cityCastle city of
    Just castle ->
      if hasLord city
        then Left LordExistError
        else Right city {cityCastle = Just castle {castleLord = Just lord}}
    Nothing -> Left MissingCastleError

buildWalls :: City -> Either WallsBuildingError City
buildWalls city =
  case cityCastle city of
    Just castle ->
      case hasLord city of
        True ->
          if (citizenCount city) < 10
            then Left NotEnoughPeopleError
            else Right
                   city {cityCastle = Just castle {castleWalls = Just Walls}}
        False -> Left NoLordError
    Nothing -> Left NoCastleError

citizenCount :: City -> Int
citizenCount city = sum $ map countCitizensInFamily families
  where
    families :: [Family] = map houseFamily housesFlattened
    houses :: Houses = cityHouses city
    housesFlattened :: [House]
    housesFlattened = housesHouse houses : housesRest houses
    countCitizensInFamily :: Family -> Int
    countCitizensInFamily family =
      1 + (length $ filter (\x -> not $ null x) (familyMembers family))
    familyMembers :: Family -> [Maybe Citizen]
    familyMembers family =
      [familyCitizen2, familyCitizen3, familyCitizen4] <*> repeat family

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
newtype FName =
  FName String

instance Eq FName where
  (==) :: FName -> FName -> Bool
  (==) (FName lhs) (FName rhs) =
    case (splitToNumericPrefix lhs, splitToNumericPrefix rhs) of 
      ((Just lhsPrefix, lhsRest), (Just rhsPrefix, rhsRest)) -> 
        (lhsPrefix == rhsPrefix) && (lowercase lhsRest == lowercase rhsRest)
      ((Nothing, lhsRest), (Nothing, rhsRest)) -> lhsRest == rhsRest
      _ -> False

instance Ord FName where
  (<=) :: FName -> FName -> Bool
  (<=) (FName lhs) (FName rhs) = 
    case (splitToNumericPrefix lhs, splitToNumericPrefix rhs) of 
      ((Just lhsPrefix, lhsRest), (Just rhsPrefix, rhsRest)) -> 
        if lhsPrefix == rhsPrefix
        then lowercase lhsRest <= lowercase rhsRest
        else lhsPrefix < rhsPrefix
      ((_, lhsRest), (_, rhsRest)) -> lowercase lhsRest <= lowercase rhsRest

splitToNumericPrefix :: String -> (Maybe Int, String)
splitToNumericPrefix s =
  case splitted of
    ("", suffix) -> (Nothing, suffix)
    (prefix, suffix) -> (Just $ read prefix, suffix)
  where
    splitToNumericPrefix' :: String -> String -> (String, String)
    splitToNumericPrefix' (c:cs) acc
      | isDigit c = splitToNumericPrefix' cs (acc ++ [c])
      | otherwise = (acc, c:cs)
    splitToNumericPrefix' "" acc = (acc, "")

    splitted = splitToNumericPrefix' s ""

lowercase :: String -> String
lowercase = map toLower

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
newtype Box a =
  Box a

data BoxD a = BoxD a

sumAndLog as (Box base) =
  let s = sum as
   in if s < 0
        then Nothing
        else Just (log s / log base)

sumAndLogD as (BoxD base) =
  let s = sum as
   in if s < 0
        then Nothing
        else Just (log s / log base)

loop = loop

main = pure ()
