{-# LANGUAGE ScopedTypeVariables #-}

module Task7 where

import Data.Either (lefts, rights)

-- null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
firstSubtask :: Bool
firstSubtask =
  (($) :: ([String] -> Bool) -> [String] -> Bool)
    ((.) (null :: [a] -> Bool) (head :: [a] -> a) :: [String] -> Bool)
    (mapExpr :: [String])
  where
    mapExpr =
      (map :: (a -> b) -> [a] -> [b])
        (uncurryExpr :: (b -> c, b) -> c)
        (listExpr :: [(String -> String, String)])
    uncurryExpr = (uncurry :: (a -> b -> c) -> (a, b) -> c) (id :: a -> a)
    listExpr = [pairExpr :: (String -> String, String)]
    pairExpr = (concatExpr :: String -> String, " Grey" :: String)
    concatExpr = (++) ("Dorian " :: String)

-- (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
secondSubtask :: [(Integer, Integer)]
secondSubtask =
  (lambda :: [Either Integer Integer] -> [(Integer, Integer)])
    (list :: [Either Integer Integer])
  where
    lambda =
      \(x :: [Either Integer Integer]) -> lambdaBody x :: [(Integer, Integer)]
    lambdaBody x = zip (lefts x :: [Integer]) (rights x :: [Integer])
    list = [(listLeft :: Either Integer b), (listRight :: Either a Integer)]
    listLeft = Left (listLeftExpr :: Integer)
    listLeftExpr = (1 :: Integer) + (2 :: Integer)
    listRight = Right (listRightExpr :: Integer)
    listRightExpr = (2 :: Integer) ^ (6 :: Integer)

-- let impl = \x y -> not x || y in
--     let isMod2 = \x -> x `mod` 2 == 0 in
--     let isMod4 = \x -> x `mod` 4 == 0 in
--     \x -> (isMod4 x) `impl` (isMod2 x)
thirdSubtask :: Integer -> Bool
thirdSubtask =
  let (impl :: Bool -> Bool -> Bool) =
        \(x :: Bool) (y :: Bool) ->
          ((||) :: Bool -> Bool -> Bool) ((not :: Bool -> Bool) x :: Bool) y
   in let (isMod2 :: Integer -> Bool) =
            \(x :: Integer) ->
              ((==) :: Integer -> Integer -> Bool)
                ((mod :: Integer -> Integer -> Integer) x (2 :: Integer) :: Integer)
                (0 :: Integer)
       in let (isMod4 :: Integer -> Bool) =
                \(x :: Integer) ->
                  ((==) :: Integer -> Integer -> Bool)
                    ((mod :: Integer -> Integer -> Integer) x (4 :: Integer) :: Integer)
                    (0 :: Integer)
           in \(x :: Integer) -> (isMod4 x) `impl` (isMod2 x)
