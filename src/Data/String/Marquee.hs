module Data.String.Marquee where

import Data.Char (isSpace)
import Data.List.Marquee

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
