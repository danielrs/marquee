module Text.Marquee.Parser (parseDoc, renderCST) where

import Control.Monad
import Data.Char (isPunctuation, isSpace)
import Data.List (foldl')
import Text.ParserCombinators.Parsec hiding (spaces, space)
import Text.ParserCombinators.Parsec.Char (oneOf, noneOf)

import Data.String.Marquee
import Text.ParserCombinators.Parsec.Marquee

import qualified Text.Marquee.CST as T

-- API

parseDoc :: Parser T.Doc
parseDoc = sepEndBy block lineEnding

renderCST :: String -> T.Doc
renderCST input = case parse parseDoc "Markdown" input of
                    Left err -> error $ show err
                    Right val -> T.group val

-- PARSING

blankLine :: Parser T.DocElement
blankLine = do
  manyTill whitespace (lookAhead lineEnding)
  return T.blankLine

-- Leaf blocks

headingUnderline :: Parser T.DocElement
headingUnderline = do
  atMostN 3 (char ' ')
  c <- try (setextOf '=') <|> setextOf '-'
  return $ T.headingUnderline (if c == '=' then 1 else 2)
  where setextOf c = manyN 3 (char c) >>= \(c:cs) -> skipMany whitespace >> lookAhead lineEnding_ >> return c

thematicBreak :: Parser T.DocElement
thematicBreak = do
  atMostN 3 (char ' ')
  try (thematicBreakOf '*') <|> try (thematicBreakOf '-') <|> thematicBreakOf '_'
  return T.thematicBreak
  where thematicBreakOf c = manyN 3 (char c >> skipMany whitespace) >> lookAhead lineEnding_

heading :: Parser T.DocElement
heading = do
  atMostN 3 (char ' ')
  pounds <- atMostN1 6 (char '#')
  (whitespace >> return ()) <|> lookAhead lineEnding_
  content <- rawInline
  return $ T.heading (length pounds) content

indented :: Parser T.DocElement
indented = do
  count 4 (char ' ')
  content <- rawInline
  return $ T.indentedLine content

fenced :: Parser T.DocElement
fenced = do
  atMostN 3 (char ' ')

  fence <- try (fenceOf '`') <|> fenceOf '~'
  let fenceLen  = length fence
      fenceChar = head fence

  infoString <- manyTill infoChar lineEnding_
  content <- manyTill anyChar $ (fenceOfN fenceLen fenceChar >> lookAhead lineEnding_) <|> eof

  return $ T.fenced infoString content
  where fenceOfN :: Int -> Char -> Parser String
        fenceOfN n c = count n (char c)
        fenceOf c    = manyN 3 (char c)
        infoChar     = notFollowedBy (char '`') >> anyChar

linkReference :: Parser T.DocElement
linkReference = do
  atMostN 3 (char ' ')
  reference <- between (char '[') (char ']') (skipMany whitespace >> many1 linkChar)
  char ':'

  skipMany whitespace >> (optional $ lineEnding >> skipMany1 whitespace)
  destination <- many1 printable

  skipMany whitespace
  title <- option "" $ parseTitle <|> (lineEnding_ >> skipMany1 whitespace >> parseTitle)

  return $ T.linkReference
            reference
            destination
            (if length title > 0 then Just title else Nothing)
  where parseTitle = printable >>= \t -> manyTill anyChar (lookAhead lineEnding_) >>= return . (:) t

paragraph :: Parser T.DocElement
paragraph = do
  content <- rawInline
  return $ T.paragraphLine content

-- Container blocks

blockquote :: Parser T.DocElement
blockquote = do
  atMostN 3 (char ' ')
  try (string "> " >> return '>') <|> char '>'
  skipMany whitespace
  content <- block
  return $ T.blockquoteLine content

unorderedList :: Parser T.DocElement
unorderedList = do
  atMostN 3 (char ' ')
  bulletMarker >> whitespace
  content <- block
  return $ T.unorderedList content

orderedList :: Parser T.DocElement
orderedList = do
  atMostN 3 (char ' ')
  num <- orderedMarker
  content <- block
  return $ T.orderedList num content

-- Inline

-- codespan :: Parser T.DocElement
-- codespan = do
--   open <- many1 (char '`')
--   content <- many codespanChar
--   count (length open) (char $ head open)
--   return $ Codespan (trim content)
--   where codespanChar = (string "\\`" >> return '`') <|> noneOf "`"

-- flanked :: Parser String -> Parser String -> Parser () -> Parser
-- T.DocElement
-- flanked open close rules = do
--   str <- open
--   notFollowedBy whitespace
--   xs <- manyTill1 (inline $ rules >> notFollowedBy (string str)) close
--   return $ foldr (<#>) Empty xs

-- bold :: Parser () -> Parser T.DocElement
-- bold rules = do
--   xs <- try (flanked (string "**") (string "**") rules)
--         <|> flanked (string "__") (string "__") rules
--   return $ Bold xs

-- italic :: Parser () -> Parser T.DocElement
-- italic rules = do
--   xs <- try (flanked (string "*") (string "*") rules)
--         <|> flanked (string "_") (string "_") rules
--   return $ Italic xs

-- link :: -> Parser T.DocElement
-- link = do
--   linkText <- between (char '[') (char ']') linkChar

-- escapedChar :: Parser T.DocElement
-- escapedChar =  notFollowedBy lineEnding >> escaped >>= return . Char
--   where escaped :: Parser Char
--         escaped = char '\\' >> satisfy isPunctuation

-- inlineChar :: Parser () -> Parser T.DocElement
-- inlineChar rules = notFollowedBy lineEnding >> rules >> anyChar >>= return . Char

block :: Parser T.DocElement
block =
  -- Leaf
  try blankLine
  <|> try headingUnderline
  <|> try thematicBreak
  <|> try heading
  <|> try indented
  <|> try fenced
  <|> try linkReference
  -- Container
  <|> try blockquote
  <|> try unorderedList
  <|> try orderedList
  <|> paragraph

rawInline :: Parser String
rawInline = manyTill anyChar (lookAhead lineEnding_)

-- inline :: Parser () -> Parser T.DocElement
-- inline rules = do
--   next <- try escapedChar
--           <|> try codespan
--           <|> try (bold rules)
--           <|> try (italic rules)
--           <|> inlineChar rules
--   rest <- many $ inline rules
--   return $ next <#> (foldr (<#>) Empty rest)

-- Parsing utilities


-- Parsing definitions

lineEnding :: Parser String
lineEnding = (string "\r\n" <|> string "\n" <|> string "\r") >>= return

lineEnding_ :: Parser ()
lineEnding_ = (lineEnding >> return ()) <|> eof

whitespace :: Parser Char
whitespace = satisfy (\c -> isSpace c && c /= '\n')

printable :: Parser Char
printable = satisfy (not . isSpace)

punctuation :: Parser Char
punctuation = satisfy isPunctuation

bulletMarker :: Parser Char
bulletMarker = oneOf "-+*"

orderedMarker :: Parser Int
orderedMarker = atMostN1 10 digit >>= return . read >>= \x -> oneOf ".)" >> return x

linkChar :: Parser Char
linkChar = (string "\\]" >> return ']') <|> noneOf "]"
