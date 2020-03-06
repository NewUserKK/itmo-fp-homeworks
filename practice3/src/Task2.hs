{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Task2 where

import Data.Monoid

mconcat :: Monoid m => [m] -> m
mconcat = foldl (<>) mempty