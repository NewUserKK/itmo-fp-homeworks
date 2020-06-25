{-# LANGUAGE BangPatterns #-}

module Task1 where

import GHC.Float (int2Double)

data Point =
  Point
    { x :: Int
    , y :: Int
    }

plus :: Point -> Point -> Point
plus p1 p2 = Point (x p1 + x p2) (y p1 + y p2)

minus :: Point -> Point -> Point
minus p1 p2 = Point (x p1 - x p2) (y p1 - y p2)

scalarProduct :: Point -> Point -> Int
scalarProduct p1 p2 = x p1 * x p2 + y p1 * y p2

crossProduct :: Point -> Point -> Int
crossProduct p1 p2 = x p1 * y p2 - x p2 * y p1

segmentLength :: Point -> Point -> Double
segmentLength from to =
  (sqrt . int2Double) $ (x to - x from) ^ (2 :: Int) + (y to - y from) ^ (2 :: Int)

foldByPairs' :: (a -> a -> b) -> (b -> b -> b) -> b -> [a] -> b
foldByPairs' _ _ !acc [] = acc
foldByPairs' _ _ !acc [_] = acc
foldByPairs' pairConvert binOp !acc (a : b : xs) =
  foldByPairs' pairConvert binOp (binOp acc $ pairConvert a b) (b : xs)

perimeter :: [Point] -> Double -- Считает периметр
perimeter list = foldByPairs' segmentLength (+) 0 (list ++ [head list])

doubleArea :: [Point] -> Int -- Считает удвоенную площадь
doubleArea list = foldByPairs' crossProduct (+) 0 (list ++ [head list])
