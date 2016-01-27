module Text.Marquee.Parser where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces, space)
import Text.ParserCombinators.Parsec.Char (oneOf, noneOf)

import Data.String.Marquee
import Text.Marquee.CST
import Text.ParserCombinators.Parsec.Marquee

blankLine :: Parser Doc
blankLine = do
  manyTill whitespace (lookAhead lineEnding)
  return BlankLine

-- Leaf blocks

thematicBreak :: Parser Doc
thematicBreak = do
  atMostN 3 (char ' ')
  try (thematicBreakOf '*') <|> try (thematicBreakOf '-') <|> thematicBreakOf '_'
  return ThematicBreak
  where thematicBreakOf c = manyN 3 (char c >> skipMany whitespace) >> lookAhead lineEnding_

heading :: Parser Doc
heading = do
  atMostN 3 (char ' ')
  pounds <- atMostN1 6 (char '#')
  whitespace
  content <- option Empty $ inline none
  return $ Heading (length pounds) content

indented :: Parser Doc
indented = do
  count 4 (char ' ')
  content <- many (notFollowedBy lineEnding_ >> anyChar)
  return $ IndentedLine content

fenced :: Parser Doc
fenced = do
  atMostN 3 (char ' ')

  fence <- try (fenceOf '`') <|> fenceOf '~'
  let fenceLen  = length fence
      fenceChar = head fence

  infoString <- manyTill infoChar lineEnding_
  content <- manyTill anyChar $ (fenceOfN fenceLen fenceChar >> lookAhead lineEnding_) <|> eof

  return $ Fenced infoString content
  where fenceOfN :: Int -> Char -> Parser String
        fenceOfN n c = count n (char c)
        fenceOf c = manyN 3 (char c)
        infoChar = notFollowedBy (char '`') >> anyChar

parseLinkReference :: Parser Doc
parseLinkReference = do
  atMostN 3 (char ' ')
  reference <- between (char '[') (char ']') (skipMany whitespace >> many1 referenceChar)
  char ':'

  skipMany whitespace >> (optional $ lineEnding >> skipMany1 whitespace)
  destination <- many1 printable

  skipMany whitespace
  title <- (parseTitle <|> (lineEnding_ >> skipMany1 whitespace >> parseTitle))

  return $ linkReference
            reference
            destination
            (if length title > 0 then Just title else Nothing)
  where referenceChar = (string "\\]" >> return ']') <|> noneOf "]"
        parseTitle = option "" $  printable >>= \t ->
                                  manyTill anyChar (lookAhead lineEnding_) >>= return . (:) t

paragraph :: Parser Doc
paragraph = do
  content <- inline none
  return $ ParagraphLine content

-- Container blocks

blockquote :: Parser Doc
blockquote = do
  atMostN 3 (char ' ')
  char '>'
  skipMany whitespace
  content <- block
  return $ BlockquoteLine content

-- Inline

codespan :: Parser Doc
codespan = do
  open <- many1 (char '`')
  content <- many codespanChar
  count (length open) (char $ head open)
  return $ Codespan (trim content)
  where codespanChar = (string "\\`" >> return '`') <|> noneOf "`"

flanked :: Parser String -> Parser String -> Parser () -> Parser Doc
flanked open close rules = do
  str <- open
  notFollowedBy whitespace
  xs <- manyTill1 (inline $ rules >> notFollowedBy (string str)) close
  return $ foldr cons Empty xs

bold :: Parser () -> Parser Doc
bold rules = do
  xs <- try (flanked (string "**") (string "**") rules)
        <|> flanked (string "__") (string "__") rules
  return $ Bold xs

italic :: Parser () -> Parser Doc
italic rules = do
  xs <- try (flanked (string "*") (string "*") rules)
        <|> flanked (string "_") (string "_") rules
  return $ Italic xs

inlineChar :: Parser () -> Parser Doc
inlineChar rules = notFollowedBy lineEnding >> rules >> anyChar >>= return . Char

block :: Parser Doc
block =
  -- Leaf
  try blankLine
  <|> try thematicBreak
  <|> try heading
  <|> try indented
  <|> try fenced
  <|> try parseLinkReference
  -- Container
  <|> try blockquote
  <|> paragraph

inline :: Parser () -> Parser Doc
inline rules = do
  next <- try codespan
          <|> try (bold rules)
          <|> try (italic rules)
          <|> inlineChar rules
  rest <- many $ inline rules
  return $ cons next (foldr cons Empty rest)

parseDoc :: Parser [Doc]
parseDoc = sepEndBy block lineEnding

renderCST :: String -> Doc
renderCST input = case parse parseDoc "Markdown" input of
                    Left err -> error $ show err
                    Right val -> foldr cons Empty val

-- Parsing utilities

lineEnding :: Parser String
lineEnding = (string "\r\n" <|> string "\n" <|> string "\r") >>= return

lineEnding_ :: Parser ()
lineEnding_ = (lineEnding >> return ()) <|> eof
