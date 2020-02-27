module Task6 where

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
distributivityWhnf = 
  ( Left ("harold" ++ " hide " ++ "the " ++ "pain")
  , Left ("harold" ++ " hide " ++ "the " ++ "pain")
  )

poleChudesWhnf :: Bool
poleChudesWhnf = False
