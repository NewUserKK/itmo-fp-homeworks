module Main where

import Task8.Constants
import Task8.Simulation

main :: IO ()
main = runWithEnvironment defaultEnv defaultStepCount
