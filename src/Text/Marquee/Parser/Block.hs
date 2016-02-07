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
html = lift optIndent *> (html1to5 <|> html6 <|> html7)
  where html1to5 = lift $ do
          xs <- H.info <|> H.comment <|> H.cdata <|> H.special <|> H.preFormated
          ys <- takeTill isLineEnding
          return . C.html $ T.append xs ys
        html6 = lift $ do
          xs <- H.simpleTag
          ys <- tillBlankLine
          return . C.html $ T.append xs ys
        html7 = lift $ do
          xs <- (H.tag <|> H.ctag) <* lookAhead whitespace
          ys <- tillBlankLine
          return . C.html $ T.append xs ys
        tillBlankLine = T.concat <$>  manyTill
                                      (Atto.takeWhile1 (not .isLineEnding) <|> Atto.take 1)
                                      (lookAhead $ lineEnding *> emptyLine)

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
