module Main where

import qualified Data.ByteString as B

import Text.Marquee
import Text.Marquee.Writers.HTML

main = do
  md <- B.readFile "app/test.md"
  let cst = renderCST md
      ast = renderAST md

  -- Show AST
  putStrLn "\n-- RESULT --\n"
  mapM_ (putStrLn . show) ast
  putStrLn "\n-- END OF RESULT --\n"

  -- Write HTML
  let html = renderHtml . writeHtml $ ast
  writeFile "app/test.html" html
