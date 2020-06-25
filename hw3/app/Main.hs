{-# LANGUAGE NumericUnderscores #-}
module Main where

import Criterion.Main
import Task1 (Point(..), doubleArea, perimeter)

square :: Int -> [Point]
square sideSize =
  generateBottom 0 sideSize
    ++ generateRight 1 sideSize
    ++ generateTop (sideSize - 1) 0
    ++ generateLeft (sideSize - 1) 1
  where
    generateBottom a b = map (`Point` 0) [a .. b]
    generateRight a b = map (Point sideSize) [a .. b]
    generateTop b a = map (`Point` sideSize) [b, b - 1 .. a]
    generateLeft b a = map (Point 0) [b, b - 1 .. a]


main :: IO ()
main = defaultMain [
    bgroup "perimeter" [
        bench "10^5" $ whnf perimeter smallSquare
      , bench "10^7" $ whnf perimeter bigSquare
      ]
  , bgroup "doubleArea" [
        bench "10^5"  $ whnf doubleArea smallSquare
      , bench "10^7"  $ whnf doubleArea bigSquare
      ]
  ]
  where
    smallSquare = square 2_500
    bigSquare = square 2_500_000
