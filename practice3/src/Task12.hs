module Task12
  ( Path (..)
  , Abs
  , Rel
  , createAbs
  , createRel
  ) where

newtype Path a =
  Path
    { unPath :: [String]
    }
  deriving (Show, Eq)

data Abs -- для абсолютного пути

data Rel -- для относительного

createAbs :: String -> Maybe (Path Abs)
createAbs pathString@(c:_) =
  case c of
    '/' -> Just $ Path $ drop 1 (splitOn '/' pathString)
    _   -> Nothing
createAbs "" = Nothing

createRel :: String -> Maybe (Path Rel)
createRel pathString@(c:_) =
  case c of
    '/' -> Nothing
    _   -> Just $ Path $ splitOn '/' pathString
createRel "" = Nothing

splitOn :: Char -> String -> [String]
splitOn splitChar s = split' s "" []
  where
    split' :: String -> String -> [String] -> [String]
    split' "" buffer result = result ++ [buffer]
    split' (c:cs) buffer result =
      if (c == splitChar)
        then split' cs "" (result ++ [buffer])
        else split' cs (buffer ++ [c]) result
