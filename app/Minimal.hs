module Main where

import Text.Marquee
import System.IO
import qualified Data.ByteString.Char8 as B

sourceMarkdown :: String
sourceMarkdown = "app/minimal.md"

sourceCss :: String
sourceCss = "minimal.css" -- relative to generated html

targetHtml :: String
targetHtml = "app/minimal.html"

main :: IO ()
main = do
  markdown <- B.readFile sourceMarkdown

  let ast = renderAST markdown
      html = renderHtml . writeHtmlDocument "Minimal" (Just sourceCss) $ ast

  putStrLn "\n-- GENERATED AST --\n"
  mapM_ (putStrLn . show) ast
  putStrLn "\n-- END OF AST --\n"

  writeFile targetHtml html
  putStrLn $ "\"" ++ targetHtml ++ "\" written!"
