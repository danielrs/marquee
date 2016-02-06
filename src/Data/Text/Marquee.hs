module Data.Text.Marquee where

import Data.Char (isSpace)
import qualified Data.Text as T

trim :: T.Text -> T.Text
trim = T.reverse . T.dropWhile isSpace . T.reverse . T.dropWhile isSpace
