{-# LANGUAGE OverloadedStrings #-}

module Text.Marquee.Parser.Block (parseBlocks) where

import Control.Applicative
import Control.Monad
import Control.Monad.State (StateT(..), get, modify, lift)

import Data.Char (isLetter, isUpper)
import Data.Text (Text())
import qualified Data.Text as T

import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator

import Text.Marquee.Parser.Common
import Text.Marquee.Parser.HTML as H
import Text.Marquee.SyntaxTrees.CST (Doc(..), DocElement(..))
import qualified Text.Marquee.SyntaxTrees.CST as C

parseBlocks :: BlockParser Doc
parseBlocks = sepBy (blankLine <|> block) (lift lineEnding)

-- Types

type BlockParser = StateT [Int] Parser

indent :: Int -> BlockParser ()
indent n = modify ((:) n)

unindent :: BlockParser ()
unindent = modify (drop 1)

indentLevel :: BlockParser Int
indentLevel = liftM sum get

-- Useful definitions

bulletMarker :: Parser Char
bulletMarker = oneOf "-+*"

orderedMarker :: Parser String
orderedMarker = atMostN1 10 digit <* oneOf ".)"

-- Parsing

block :: BlockParser DocElement
block = choice [
  headingUnderline
  , thematicBreak
  , heading
  , indentedLine
  , fenced
  , html
  , linkRef
  -- Container
  , blockquote
  , unorderedList
  , orderedList
  -- -- Any other is a paragraph
  , paragraph
  ]

indented :: BlockParser DocElement -> BlockParser DocElement
indented p = do
  (blankLine >> lift lineEnding >> blankLine >> return C.blankLine)
  <|>
  (blankLine <|> (indentLevel >>= \level -> count level (lift linespace) >> p))

blankLine :: BlockParser DocElement
blankLine = lift $ next (lookAhead lineEnding_) *> return C.blankLine

headingUnderline :: BlockParser DocElement
headingUnderline = lift $ do
  optIndent
  cs@(c:_) <- setextOf 1 '=' <|> setextOf 2 '-'
  return $ C.headingUnderline (if c == '=' then 1 else 2) (T.pack cs)
  where setextOf n c = manyN n (char c) <* next (lookAhead lineEnding_)

thematicBreak :: BlockParser DocElement
thematicBreak = lift $ do
  optIndent
  thematicBreakOf '-' <|> thematicBreakOf '_' <|> thematicBreakOf '*'
  return C.thematicBreak
  where thematicBreakOf c = manyN 3 (char c <* skipWhile isLinespace) <* next (lookAhead lineEnding_)

heading :: BlockParser DocElement
heading = lift $ do
  optIndent
  pounds <- atMostN1 6 (char '#')
  lookAhead (void linespace) <|> lookAhead lineEnding_
  content <- headingInline (length pounds)
  return $ C.heading (length pounds) content

indentedLine :: BlockParser DocElement
indentedLine = lift $ C.indentedBlock <$> (string "    " *> rawInline)

fenced :: BlockParser DocElement
fenced = do
  indentLen <- (+) <$> indentLevel <*> (length <$> lift optIndent)
  lift $ do
    fence <- fenceOf '`' <|> fenceOf '~'
    infoString <-  Atto.takeTill (\c -> c == '`' || isLineEnding c) <* lineEnding
    let fenceIndent  = atMostN indentLen (char ' ')
        closingFence = fenceIndent *> optIndent *> manyN (length fence) (char $ head fence)

    content <-  manyTill
                (fenceIndent *> Atto.takeTill isLineEnding <* lineEnding_)
                (endOfInput <|> lookAhead (closingFence *> next lineEnding_))
    closingFence *> takeTill isLineEnding
    return $ C.fenced infoString content
  where fenceOf c = manyN 3 (char c)

html :: BlockParser DocElement
html = html1 <|> html2 <|> html3 <|> html4 <|> html5 <|> html6 <|> html7

linkRef :: BlockParser DocElement
linkRef = lift $ do
  optIndent
  ref    <- linkLabel <* char ':'
  dest   <- spacing *> linkDestination
  mtitle <- (optionMaybe $ spacing *> linkTitle) <* next (lookAhead lineEnding_)

  return $ C.linkReference ref dest mtitle
  where spacing = skipWhile isLinespace >> (optional $ lineEnding *> skipMany linespace)

paragraph :: BlockParser DocElement
paragraph = lift $ C.paragraphBlock <$> rawInline

blockquote :: BlockParser DocElement
blockquote = C.blockquoteBlock <$> (lift (optIndent *> char '>' *> skipWhile isLinespace) *> block)

unorderedList :: BlockParser DocElement
unorderedList = do
  indent <- lift $ length <$> optIndent <* bulletMarker
  spaces <- lift $ T.length <$> takeWhile1 isLinespace
  C.unorderedList <$> listItem (indent + 1 + spaces)

orderedList :: BlockParser DocElement
orderedList = do
  indent <- lift $ length <$> optIndent
  num <- lift orderedMarker
  spaces <- lift $ T.length <$> takeWhile1 isLinespace
  C.orderedList (read num) <$> listItem (indent + length num + 1 + spaces)

listItem :: Int -> BlockParser Doc
listItem indentAmount =
  indent indentAmount *> ((:) <$> block <*> (many $ lift lineEnding >> indented block)) <* unindent

rawInline :: Parser Text
rawInline = Atto.takeTill isLineEnding

headingInline :: Int -> Parser Text
headingInline headingSize = do
  xs <- manyTill anyChar (lookAhead end) <* end
  return $ T.pack xs
  where end = lookAhead lineEnding_ <|> linespace *> Atto.takeWhile (== '#') *> next (lookAhead lineEnding_)

-- HTML

html1 :: BlockParser DocElement
html1 = lift $ do
  optIndent
  open <- opening
  xs <- T.singleton <$> (linespace <|> char '>' <|> satisfy isLineEnding)
  ys <- T.concat <$> manyTill (content) (lookAhead closing)
  close <- closing
  return . C.html . T.concat $ [open, xs, ys, close]
  where opening = string "<script" <|> string "<pre" <|> string "<style"
        closing = string "</script>" <|> string "</pre>" <|> string "</style>" <|> (endOfInput >> return "")
        content = Atto.takeWhile1 (/= '<') <|> Atto.take 6

html2 :: BlockParser DocElement
html2 = lift $ do
  optIndent
  open <- opening
  xs <- T.concat <$> manyTill (Atto.takeWhile1 (/= '-') <|> Atto.take 3) (lookAhead closing)
  close <- closing
  return . C.html . T.concat $ [open, xs, close]
  where opening = string "<!--"
        closing = string "-->" <|> (endOfInput >> return "")

html3 :: BlockParser DocElement
html3 = lift $ do
  optIndent
  open <- opening
  xs <- T.concat <$> manyTill (Atto.takeWhile1 (/= '?') <|> Atto.take 2) (lookAhead closing)
  close <- closing
  return . C.html . T.concat $ [open, xs, close]
  where opening = string "<?"
        closing = string "?>" <|> (endOfInput >> return "")

html4 :: BlockParser DocElement
html4 = lift $ do
  optIndent
  open <- opening
  xs <- T.concat <$> manyTill (Atto.takeWhile1 (/= '>') <|> Atto.take 1) (lookAhead closing)
  close <- closing
  return . C.html . T.concat $ [open, xs, close]
  where opening = T.snoc <$> string "<!" <*> satisfy (\c -> isLetter c && isUpper c)
        closing = string ">" <|> (endOfInput >> return "")

html5 :: BlockParser DocElement
html5 = lift $ do
  optIndent
  open <- opening
  xs <- T.concat <$> manyTill (Atto.takeWhile1 (/= ']') <|> Atto.take 3) (lookAhead closing)
  close <- closing
  return . C.html . T.concat $ [open, xs, close]
  where opening = string "<![CDATA["
        closing = string "]]>" <|> (endOfInput >> return "")

html6 :: BlockParser DocElement
html6 = lift $ do
  optIndent
  a <- string "</" <|> string "<"
  b <- choice $ map string tags
  c <- T.singleton <$> (linespace <|> char '>' <|> satisfy isLineEnding) <|> string "/>"
  xs <- T.concat
        <$> manyTill (Atto.takeWhile1 (not . isLineEnding) <|> Atto.take 1) (lookAhead closing)
  return . C.html $ T.concat [a, b, c, xs]
  where closing = endOfInput <|> lineEnding >> emptyLine

html7 :: BlockParser DocElement
html7 = lift $ do
  optIndent
  tag <- H.tag <|> H.ctag
  tag' <- T.singleton <$> lookAhead whitespace
  xs <- T.concat
        <$> manyTill (Atto.takeWhile1 (not . isLineEnding) <|> Atto.take 1) (lookAhead closing)
  return . C.html $ T.concat [tag, tag', xs]
  where closing = endOfInput <|> lineEnding *> emptyLine

tags :: [Text]
tags =  ["address"
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
