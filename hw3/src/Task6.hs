{-# LANGUAGE Rank2Types #-}

module Task6 where

import Task5.FS
import Lens.Micro

--    cd: перейти в поддиректорию с указанным именем.
--    ls: получить список имён содержимого директории.
--    file: получить имя конкретного File, если он существует.
--
--В итоге должна быть возможность делать нечто похожее:
--
--myDir ^?  cd "A" . cd "B" . file "C"  -- Just "C" при существовании myDir/A/B/C
--myDir ^.. cd "A" . cd "B" . ls        -- получить содержимое myDir/A/B/

cd :: String -> Traversal' FS FS
cd cdName = _Dir . contents . traversed . (filtered (\dir -> dir ^. _Dir . name == cdName))

ls :: Traversal' FS String
ls = _Dir . contents . traversed . name

file :: String -> Traversal' FS String
file fileName = _Dir . contents . traversed . (filtered (\f -> f ^. _File . name == fileName)) . name