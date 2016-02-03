module Data.ByteString.Marquee where

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as B

trim :: B.ByteString -> B.ByteString
trim = B.reverse . B.dropWhile isSpace . B.reverse . B.dropWhile isSpace
