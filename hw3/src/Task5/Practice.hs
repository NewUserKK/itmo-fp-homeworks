module Task5.Practice where

import Task5.FS
import Lens.Micro

lsOrEmpty :: FS -> [FS]
lsOrEmpty dir = dir ^. _Dir . contents

maybeDirName :: FS -> Maybe String
maybeDirName dir = dir ^? _Dir . name

fileNameOrEmpty :: FS -> FilePath
fileNameOrEmpty file = file ^. _File . name 

changeRootName :: FS -> FS
changeRootName = _Dir . name .~ "/"

addSuffix :: String -> FS -> FS
addSuffix suffix = _Dir . name %~ (++ suffix)

firstDirName :: FS -> Maybe String
firstDirName dir = dir ^? _Dir . contents . traversed . _Dir . name

fileNames :: FS -> [String]
fileNames dir = dir ^.. _Dir . contents . traversed . _File . name