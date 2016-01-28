module Main where

import Data.List
import System.IO

import Text.Marquee

main = do
  md <- readFile "app/test.md"
  putStrLn "\n-- RESULT --\n"
  mapM_ (putStrLn . show) $ renderCST $ concat $ replicate 1 md
  putStrLn "\n-- END OF RESULT --\n"
