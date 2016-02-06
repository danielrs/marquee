{-# LANGUAGE OverloadedStrings #-}

module Text.Marquee.Parser.Block (parseBlocks) where

import Control.Applicative
import Control.Monad
import Control.Monad.State (StateT(..), get, modify, lift)

import Data.Text (Text())
import qualified Data.Text as T

import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator

import Text.Marquee.Parser.Common
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
  (blankLine >> lift lineEnding >> blankLine >> lift lineEnding >> return C.blankLine)
  <|>
  (blankLine <|> (indentLevel >>= \level -> count level (lift whitespace) >> p))

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
  where thematicBreakOf c = manyN 3 (char c <* skipWhile isWhitespace) <* next (lookAhead lineEnding_)

heading :: BlockParser DocElement
heading = lift $ do
  optIndent
  pounds <- atMostN1 6 (char '#')
  lookAhead (void whitespace) <|> lookAhead lineEnding_
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
    let closingFence = manyN (length fence) (char $ head fence)
        fenceIndent  = atMostN indentLen (char ' ')
    content <-  manyTill
                (fenceIndent *> Atto.takeTill isLineEnding <* lineEnding_)
                (endOfInput <|> (fenceIndent *> optIndent *> closingFence *> next (lookAhead lineEnding_)))
    return $ C.fenced infoString content
  where fenceOf c = manyN 3 (char c)

linkRef :: BlockParser DocElement
linkRef = lift $ do
  optIndent
  ref    <- linkLabel <* char ':'
  dest   <- spacing *> linkDestination
  mtitle <- (optionMaybe $ spacing *> linkTitle) <* next (lookAhead lineEnding_)

  return $ C.linkReference ref dest mtitle
  where spacing = skipWhile isWhitespace >> (optional $ lineEnding *> skipMany whitespace)

paragraph :: BlockParser DocElement
paragraph = lift $ C.paragraphBlock <$> rawInline

blockquote :: BlockParser DocElement
blockquote = C.blockquoteBlock <$> (lift (optIndent *> char '>' *> skipWhile isWhitespace) *> block)

unorderedList :: BlockParser DocElement
unorderedList = do
  indent <- lift $ length <$> optIndent <* bulletMarker
  spaces <- lift $ T.length <$> takeWhile1 isWhitespace
  C.unorderedList <$> listItem (indent + 1 + spaces)

orderedList :: BlockParser DocElement
orderedList = do
  indent <- lift $ length <$> optIndent
  num <- lift orderedMarker
  spaces <- lift $ T.length <$> takeWhile1 isWhitespace
  C.orderedList (read num) <$> listItem (indent + length num + 1 + spaces)

listItem :: Int -> BlockParser Doc
listItem indentAmount =
  indent indentAmount *> ((:) <$> block <*> (many $ lift lineEnding >> indented block)) <* unindent

rawInline :: Parser Text
rawInline = Atto.takeTill isLineEnding

headingInline :: Int -> Parser Text
headingInline headingSize = do
  xs <- manyTill anyChar (lookAhead $ lineEnding <|> (whitespace >> many (char '#') >> lineEnding_))
  return $ T.pack xs
