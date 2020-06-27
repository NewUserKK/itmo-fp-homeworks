{-# LANGUAGE TemplateHaskell #-}

module Task8.Cell where

import Lens.Micro ((^.))
import Lens.Micro.Platform
import System.Random

data Cell = Cell
  { _status :: HumanStatus
  , _random :: StdGen
  }

data HumanStatus
  = Healthy
  | Incubation
    { _daysRemaining :: Int
    }
  | Sick
    { _daysRemaining :: Int
    }
  | Immune
    { _daysRemaining :: Int
    }

makeLenses ''HumanStatus
makeLenses ''Cell

instance Show Cell where
  show cell =
    case (cell^.status) of
      Healthy -> " "
      Incubation{} -> "*"
      Sick{} -> "#"
      Immune{} -> "@"

