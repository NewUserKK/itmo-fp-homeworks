{-# LANGUAGE InstanceSigs #-}

module Task8.ListZipper where

import Control.Comonad
import GHC.Base (divInt)
import Task8.Constants

data ListZipper a =
  ListZipper [a] a [a]

instance Show a => Show (ListZipper a) where
  show = toString (horizontalGridSize `divInt` 2)

instance Functor ListZipper where
  fmap f (ListZipper ls x rs) = ListZipper (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (ListZipper _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight

listLeft :: ListZipper a -> ListZipper a
listLeft (ListZipper (a:as) x bs) = ListZipper as a (x : bs)
listLeft _ = error "Cannot move left"

listRight :: ListZipper a -> ListZipper a
listRight (ListZipper as x (b:bs)) = ListZipper (x : as) b bs
listRight _ = error "Cannot move right"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (ListZipper ls _ rs) = ListZipper ls x rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (z a -> z a) -> (z a -> z a) -> z a -> ListZipper (z a)
genericMove f g e = ListZipper (iterateTail f e) e (iterateTail g e)

toString :: Show a => Int -> ListZipper a -> String
toString size (ListZipper as x bs) =
  (concatMap show . reverse $ take size as) ++
  show x ++
  (concatMap show $ take size bs) ++
  "\n"
