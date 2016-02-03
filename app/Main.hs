{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.List (intercalate)
import Options.Applicative
import System.IO
import qualified Data.ByteString as B

import Text.Marquee

-- Options

data Options = Options
  { input     :: Maybe String
  , output    :: Maybe String
  , format    :: Maybe String
  , htmlPage  :: Bool
  , htmlTitle :: Maybe String
  , htmlCss   :: Maybe String } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> (optional $
        strOption (short 'i'
                  <> long "input"
                  <> metavar "INPUT_FILE"
                  <> help "Markdown file to read"))

  <*> (optional $
        strOption (short 'o'
                  <> long "output"
                  <> metavar "OUTPUT_FILE"
                  <> help "File to write the output to"))

  <*> (optional $
        strOption (short 'f'
                  <> long "format"
                  <> metavar "FORMAT"
                  <> help ("The format of the output, supported formats are: " ++ intercalate "," formats)))

  <*> (switch (long "html-page" <>  help "Produce HTML as a full page (doctype, head, and body)"))

  <*> (optional $
        strOption (long "html-title"
                  <> metavar "TITLE"
                  <> help "The title of the generated HTML page"))

  <*> (optional $
        strOption (long "html-css"
                  <> metavar "CSS_FILE"
                  <> help "The CSS file that will be linked to the generated HTML page"))

-- Supported formats

data Format = FormatHtml

instance Show Format where
  show FormatHtml = "html"

fromString :: String -> Format
fromString "html" = FormatHtml
fromString _      = FormatHtml

formats :: [String]
formats = map show [FormatHtml]

-- Main

main = execParser opts >>= main'
  where opts = info (helper <*> optionsParser)
          (fullDesc
          <> header "Marquee: Markdown transpiler"
          <> progDesc "Reads Markdown and output something else")

main' :: Options -> IO ()
main' options = do
  input <- maybe (return stdin) (flip openFile ReadMode) (input options)
  output <- maybe (return stdout) (flip openFile WriteMode) (output options)
  let outputFormat = maybe FormatHtml fromString (format options)

  (dispatch outputFormat) options input output

  hClose output
  hClose input

-- Outputs

dispatch :: Format -> (Options -> Handle -> Handle -> IO ())
dispatch _ = outputHtml

outputHtml :: Options -> Handle -> Handle -> IO ()
outputHtml options input output = do
  markdown <- B.hGetContents input

  let writer =  if htmlPage options
                  then writeHtmlDocument (fromMaybe "" (htmlTitle options)) (htmlCss options)
                  else writeHtml
      ast  = renderAST markdown
      html = renderHtml . writer $ ast

  hPutStrLn output html
