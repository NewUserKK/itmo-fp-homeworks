{-# LANGUAGE ScopedTypeVariables #-}

module Task5.Path where

import Data.List (intercalate)
import Data.List.NonEmpty as NE

type Path = NonEmpty String

type StringPath = String

nameByPath :: Path -> String
nameByPath = NE.last

stringToPath :: StringPath -> Path
stringToPath "/" = "/":|[]
stringToPath ('/':cs) = "/" :| NE.toList (splitOn '/' cs)
stringToPath s = splitOn '/' s

pathToString :: Path -> StringPath
pathToString ("/":|[]) = "/"
pathToString ("/":|cs) = "/" ++ (intercalate "/" cs)
pathToString path = intercalate "/" . NE.toList $ path

getParentPath :: Path -> Path
getParentPath root@("/" :| []) = root
getParentPath list = NE.fromList $ NE.init list

isParentOf :: Path -> Path -> Bool
isParentOf parent path = (NE.toList parent) `NE.isPrefixOf` path

concatPath :: FilePath -> FilePath -> FilePath
concatPath "" path = path
concatPath parentPath "" = parentPath
concatPath parentPath path = parentPath ++ "/" ++ path

(</>) ::  FilePath -> FilePath -> FilePath
(</>) = concatPath

emptyPath :: Path
emptyPath = "" :| []

-- | Split list of values by given value.
-- Return non-empty list of lists of stored parts.
splitOn :: forall a . Eq a => a -> [a] -> NonEmpty [a]
splitOn splitElement list =
  case splitResult of
    Just result -> result
    Nothing -> [] :| []
  where
    splitResult :: Maybe (NonEmpty [a])
    splitResult = nonEmpty $ (buffer : splitted)

    (splitted, buffer) = foldr foldFunc ([], []) list

    foldFunc :: a -> ([[a]], [a]) -> ([[a]], [a])
    foldFunc x acc
      | x == splitElement = ((snd acc : fst acc), [])
      | otherwise = (fst acc, x : snd acc)

(<:|) :: NonEmpty a -> a -> NonEmpty a
(<:|) ne a = ne <> (a :| [])