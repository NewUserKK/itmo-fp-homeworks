module Task6 where

import Task1 (distributivity)

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True -> Just $ exp pi
    False -> Nothing

-- not actually, but good enough for this task
null :: [a] -> Bool
null [] = True
null _ = False

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
  let rs = mapMaybe f xs
    in case f x of
      Nothing -> rs
      Just r -> r : rs

distributivityWhnf :: (Either String b, Either String c)
distributivityWhnf = (a, b)
  where
    dist = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))
    a = fst dist
    b = snd dist

poleChudesWhnf :: Bool
poleChudesWhnf = False
