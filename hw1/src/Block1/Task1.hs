{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Block1.Task1
  ( Weekday(..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

-- | Structure representing days of a week
data Weekday = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving Show

instance Enum Weekday where
  fromEnum :: Weekday -> Int
  fromEnum =
    \case
      Monday -> 0
      Tuesday -> 1
      Wednesday -> 2
      Thursday -> 3
      Friday -> 4
      Saturday -> 5
      Sunday -> 6

  toEnum :: Int -> Weekday
  toEnum =
    \case
      0 -> Monday
      1 -> Tuesday
      2 -> Wednesday
      3 -> Thursday
      4 -> Friday
      5 -> Saturday
      6 -> Sunday
      _ -> error "Can't convert value to enum"

instance Eq Weekday where
  (==) :: Weekday -> Weekday -> Bool
  (==) lhs rhs = fromEnum lhs == fromEnum rhs

-- | Return the next day of the week
nextDay :: Weekday -> Weekday
nextDay weekday = afterDays weekday 1

-- | Return day of the week that is after certain amount of days after given one
afterDays :: Weekday -> Int -> Weekday
afterDays weekday after = toEnum $ (fromEnum weekday + after) `mod` 7

-- | Check if the day is weekend. 
-- Weekend days are commonly expected to be Saturday and Sunday 
isWeekend :: Weekday -> Bool
isWeekend weekday =
  case weekday of
    Saturday -> True
    Sunday   -> True
    _        -> False

-- | Return number of days before Friday
daysToParty :: Weekday -> Int
daysToParty weekday = (toEnum $ 11 - fromEnum weekday) `mod` 7
