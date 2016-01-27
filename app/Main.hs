module Main where

import Data.List
import System.IO (putStrLn)

import Text.Marquee

testMarkdown :: String
testMarkdown =  unlines [
                "# Coding guidelines"
                , "[   zero]:"
                , " zero.com"
                , " as"
                , "\\**Carefulness** is required, follow the _guidelines_:"
                , "```javascript"
                , "let i = 0;"
                , "++i;"
                , "console.log(i);"
                , "```"
                , "    Forgot this one"
                , "---"
                , "[one]: one.com Uno"]

main = do
  putStrLn $ show $ renderCST $ unlines $ replicate 1 testMarkdown
