{-# LANGUAGE InstanceSigs #-}

module Task8.Grid where

import Control.Comonad
import GHC.Base (divInt)
import Task8.Constants
import Task8.ListZipper

newtype Grid a =
  Grid
    { unGrid :: ListZipper (ListZipper a)
    }

instance Show a => Show (Grid a) where
  show :: Grid a -> String
  show = toString (verticalGridSize `divInt` 2) . unGrid

instance Functor Grid where
  fmap f (Grid grid) = Grid $ (fmap f) <$> grid

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead

  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical

up :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

down :: Grid a -> Grid a
down (Grid g) = Grid (listRight g)

left :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)

right :: Grid a -> Grid a
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right

vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove up down
