module Main where

import Data.List
import System.IO

import Text.Marquee
import Text.Marquee.Writers.HTML

main = do
  md <- readFile "app/test.md"

  -- Show AST
  putStrLn "\n-- RESULT --\n"
  mapM_ (putStrLn . show) $ renderCST $ concat $ replicate 1 md
  putStrLn "\n-- END OF RESULT --\n"

  -- Write HTML
  let html = renderHtml . writeHtml . renderAST $ concat $ replicate 1 md
  writeFile "app/test.html" html
