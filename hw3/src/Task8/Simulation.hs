{-# LANGUAGE LambdaCase #-}
module Task8.Simulation where

import Control.Comonad
import Control.Monad.Trans.Reader
import Lens.Micro.Platform
import System.Console.ANSI (clearScreen)
import System.Random (StdGen, mkStdGen, randomR)
import Task8.Cell
import Task8.Constants
import Task8.Grid
import Task8.ListZipper
import Time (threadDelay)
import Time.Units (sec)

type Seed = Int

data Environment = Environment
  { contaminationProbability :: Double
  , incubationPeriod :: Int
  , symptomaticPeriod :: Int
  , immunePeriod :: Int
  }

defaultEnv :: Environment
defaultEnv = Environment
  { contaminationProbability = defaultContaminationProbability
  , incubationPeriod = defaultIncubationPeriod
  , symptomaticPeriod = defaultSymptomaticPeriod
  , immunePeriod = defaultImmunePeriod
  }

runWithEnvironment :: Environment -> Int -> IO ()
runWithEnvironment env = runSimulation (runReader initialGrid env) env

runSimulation :: Grid Cell -> Environment -> Int -> IO ()
runSimulation grid _ 0 = print grid
runSimulation grid env steps = do
  clearScreen
  print grid
  threadDelay $ sec 0.1
  runSimulation (nextStep env grid) env (steps - 1)

initialGrid :: Reader Environment (Grid Cell)
initialGrid = do
  sickDays <- asks symptomaticPeriod
  let seed = 136461419 :: Seed
  return $ Grid $
    ListZipper
      (flip map (iterateWithDelta 29 seed) $ \centralSeed ->
        genLine centralSeed (healthyCell centralSeed))
      (genLine seed (sickCell sickDays seed))
      (flip map (iterateWithDelta 53 seed) $ \centralSeed ->
        genLine centralSeed (healthyCell centralSeed))
  where
    genLine seed centerCell =
      ListZipper
        (map (healthyCell) (iterateWithDelta 59 seed))
        centerCell
        (map (healthyCell) (iterateWithDelta 37 seed))

    iterateWithDelta delta initial = iterate (+delta) initial

nextStep :: Environment -> Grid Cell -> Grid Cell
nextStep env = extend (\g -> runReader (rule g) env)

rule :: Grid Cell -> Reader Environment Cell
rule grid = do
  let cell = extract grid
  sickDays <- asks symptomaticPeriod
  immuneDays <- asks immunePeriod
  case (cell^.status) of
    Healthy -> updateHealthyCell grid
    Incubation _ -> return $ updateNonHealthyCell cell (status .~ Sick sickDays $ cell)
    Sick _ -> return $ updateNonHealthyCell cell (status .~ Immune immuneDays $ cell)
    Immune _ -> return $ updateNonHealthyCell cell (status .~ Healthy $ cell)

updateHealthyCell :: Grid Cell -> Reader Environment Cell
updateHealthyCell grid = do
  let cell = extract grid
  probability <- asks contaminationProbability
  incubationDays <- asks incubationPeriod
  let (wasContaminated, newGen) = tryInfest (infectedNeighboursCount grid) (cell^.random) probability
  return $ random .~ newGen $
    if (wasContaminated)
     then status .~ Incubation incubationDays $ cell
     else cell

tryInfest :: Int -> StdGen -> Double -> (Bool, StdGen)
tryInfest 0 gen _ = (False, gen)
tryInfest n gen probability = do
  let (rand, newGen) = randomR (0.0, 1.0) gen
  let wasContaminated = rand <= probability
  if wasContaminated
   then (wasContaminated, newGen)
   else tryInfest (n - 1) newGen probability

infectedNeighboursCount :: Grid Cell -> Int
infectedNeighboursCount g =
  infectedCount $ map (\direction -> extract $ direction g) neighbours

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where
    horizontals = [left, right]
    verticals = [up, down]

infectedCount :: [Cell] -> Int
infectedCount = length . filter isInfected
  where
    isInfected :: Cell -> Bool
    isInfected cell =
      case cell^.status of
        Incubation{} -> True
        Sick{} -> True
        _ -> False

updateNonHealthyCell :: Cell -> Cell -> Cell
updateNonHealthyCell cell nextCell =
  case cell^?!status.daysRemaining of
    n | n <= 1 -> nextCell
    _ -> status . daysRemaining %~ (`subtract` 1) $ cell

healthyCell :: Seed -> Cell
healthyCell seed = status .~ Healthy $ emptyCell seed

incubationCell :: Int -> Seed -> Cell
incubationCell days seed = status .~ Incubation days $ emptyCell seed

sickCell :: Int -> Seed -> Cell
sickCell sickDays seed = status .~ Sick sickDays $ emptyCell seed

immuneCell :: Int -> Seed -> Cell
immuneCell immuneDays seed = status .~ Immune immuneDays $ emptyCell seed

emptyCell :: Seed -> Cell
emptyCell seed = Cell
  { _status = Healthy
  , _random = mkStdGen seed
  }
