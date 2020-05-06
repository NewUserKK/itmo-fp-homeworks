module Path where

import Data.List.NonEmpty as NE
import Utils
import Data.List (intercalate)

type Path = NonEmpty String

type StringPath = String

nameByPath :: Path -> String
nameByPath = NE.last

stringToPath :: StringPath -> Path
stringToPath "/" = "/":|[]
stringToPath ('/':cs) = "/" :| NE.drop 1 (splitOn '/' cs)
stringToPath s = splitOn '/' s

pathToString :: Path -> StringPath
pathToString ("/":|[]) = "/"
pathToString ("/":|cs) = "/" ++ (intercalate "/" cs)
pathToString path = intercalate "/" . NE.toList $ path

extensionFromPath :: Path -> String
extensionFromPath = NE.last . splitOn '.' . NE.last

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