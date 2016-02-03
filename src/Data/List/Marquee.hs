module Data.List.Marquee where

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if null xs && p x then [] else x : xs) []

lookupOr :: (Eq key) => a -> key -> [(key, a)] -> a
lookupOr backup key xs =
  case lookup key xs of
    Nothing -> backup
    Just x -> x

