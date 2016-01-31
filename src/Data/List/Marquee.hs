module Data.List.Marquee where

import Data.List (lookup)

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if null xs && p x then [] else x : xs) []

lookupOr :: (Eq b) => a -> b -> [(b, a)] -> a
lookupOr backup key assocs = case lookup key assocs of
  Nothing -> backup
  Just x -> x
