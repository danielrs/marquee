{-# LANGUAGE OverloadedStrings #-}

module Text.Marquee.Parser.HTML (
  info
  , comment
  , cdata
  , special
  , preFormated
  , simpleTag
  , tag
  , ctag) where

import Control.Applicative
import Control.Monad

import Data.Char (isLetter, isDigit)
import Data.Text(Text())
import qualified Data.Text as T

import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator

import Text.Marquee.Parser.Common

info :: Parser Text
info = do
  start   <- startP
  content <- T.concat <$> manyTill (Atto.takeWhile1 (/= '?') <|> Atto.take 2) (lookAhead endP)
  end     <- endP
  return $ T.concat [start, content, end]
  where startP = string "<?"
        endP   = string "?>"

comment :: Parser Text
comment = do
  start   <- startP
  content <- T.concat <$> manyTill (Atto.takeWhile1 (/= '-') <|> Atto.take 3) (lookAhead endP)
  end     <- endP
  return $ T.concat [start, content, end]
  where startP = string "<!--"
        endP   = string "-->"

cdata :: Parser Text
cdata = do
  start   <- startP
  content <- T.concat <$> manyTill (Atto.takeWhile1 (/= ']') <|> Atto.take 3) (lookAhead endP)
  end     <- endP
  return $ T.concat [start, content, end]
  where startP = string "<![CDATA["
        endP   = string "]]>"

special :: Parser Text
special = do
  start   <- startP
  content <- T.concat <$> manyTill (Atto.takeWhile1 (/= '>') <|> Atto.take 1) (lookAhead endP)
  end     <- endP
  return $ T.concat [start, content, end]
  where startP = string "<!"
        endP   = string ">"

preFormated :: Parser Text
preFormated = do
  start   <- startP
  content <- T.concat <$> manyTill (Atto.takeWhile1 (/= '<') <|> Atto.take 6) (lookAhead endP)
  end     <- endP
  return $ T.concat [start, content, end]
  where startP = stringCI "<script" <|> stringCI "<pre" <|> stringCI "<style"
        endP   = stringCI "</script>" <|> stringCI "</pre>" <|> stringCI "</style>"

simpleTag :: Parser Text
simpleTag = do
  start <- startP
  tname <- choice $ map stringCI tagCache
  end   <- endP
  return $ T.concat [start, tname, end]
  where startP = string "<" <|> string "</"
        endP   = T.singleton <$> whitespace <|> string ">" <|> string "/>"

-- Any valid tag

tag :: Parser Text
tag = do
  open <- string "<"
  tname <- tagName
  tattrs <- T.concat <$> many tagAttribute
  space <- Atto.takeWhile isWhitespace
  close <- string "/>" <|> string ">"
  return $ T.concat [open, tname, tattrs, space, close]

ctag :: Parser Text
ctag = do
  open <- string "</"
  tname <- tagName
  space <- Atto.takeWhile isWhitespace
  close <- string ">"
  return $ T.concat [open, tname, space, close]

tagName :: Parser Text
tagName = do
  x <- satisfy isLetter
  xs <- Atto.takeWhile (\c -> isLetter c || isDigit c || c == '-')
  return $ T.cons x xs

tagAttribute :: Parser Text
tagAttribute = do
  space <- Atto.takeWhile1 isWhitespace
  name <- attributeName
  value <- option "" attributeValueSpec
  return $ T.concat [space, name, value]


attributeName :: Parser Text
attributeName = do
  x <- satisfy (\c -> isLetter c || c `elem` ("_:" :: String))
  xs <- Atto.takeWhile (\c -> isLetter c || isDigit c || c  `elem` ("_:.-" :: String))
  return $ T.cons x xs

attributeValueSpec :: Parser Text
attributeValueSpec = do
  space0 <- Atto.takeWhile isWhitespace
  eq <- string "="
  space1 <- Atto.takeWhile isWhitespace
  val <- attributeValue
  return $ T.concat [space0, eq, space1, val]

attributeValue :: Parser Text
attributeValue = unquoted <|> quoted '\'' <|> quoted '"'
  where unquoted = Atto.takeWhile1 (flip notElem ("\"'=<>`" :: String))
        quoted c = do
          open <- T.singleton <$> char c
          xs <- Atto.takeWhile (/= c)
          close <- T.singleton <$> char c
          return $ T.concat [open, xs, close]

-- Tag cache

tagCache :: [Text]
tagCache =  ["address"
        , "article"
        , "aside"
        , "base"
        , "basefont"
        , "blockquote"
        , "body"
        , "caption"
        , "center"
        , "col"
        , "colgroup"
        , "dd"
        , "details"
        , "dialog"
        , "dir"
        , "div"
        , "dl"
        , "dt"
        , "fieldset"
        , "figcaption"
        , "figure"
        , "footer"
        , "form"
        , "frame"
        , "frameset"
        , "h1"
        , "head"
        , "header"
        , "hr"
        , "html"
        , "iframe"
        , "legend"
        , "li"
        , "link"
        , "main"
        , "menu"
        , "menuitem"
        , "meta"
        , "nav"
        , "noframes"
        , "ol"
        , "optgroup"
        , "option"
        , "p"
        , "param"
        , "section"
        , "source"
        , "summary"
        , "table"
        , "tbody"
        , "td"
        , "tfoot"
        , "th"
        , "thead"
        , "title"
        , "tr"
        , "track"
        , "ul"]
