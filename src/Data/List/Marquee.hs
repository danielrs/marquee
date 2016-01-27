module Data.List.Marquee where

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if null xs && p x then [] else x : xs) []
