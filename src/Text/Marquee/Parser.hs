{-# LANGUAGE OverloadedStrings #-}

module Text.Marquee.Parser (renderCST, renderAST) where

-- Control imports
import Control.Arrow (first, second)
import Control.Applicative
import Control.Monad
import Control.Monad.State (StateT(..), get, modify, lift)

-- Data imports
import Data.Char (isControl, isPunctuation)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M (lookup)
import qualified Data.Set as S

-- Attoparsec imports
import Data.Attoparsec.ByteString.Char8 as Atto
import Data.Attoparsec.Combinator

-- Own imports
import Data.ByteString.Marquee
import qualified Text.Marquee.SyntaxTrees.CST as C
import qualified Text.Marquee.SyntaxTrees.AST as A

renderCST :: B.ByteString -> C.Doc
renderCST input = case parse parseDoc input of
                    Left err -> []
                    Right val -> C.clean . fst $ val
  where parse p = parseOnly (runStateT p [])

renderAST :: B.ByteString -> A.Markdown
renderAST input = uncurry fromDoc $ A.stripLinkReferences $ renderCST input

fromDoc :: A.LinkMap -> C.Doc -> A.Markdown
fromDoc linkMap = map (fromDocElement linkMap)

fromDocElement :: A.LinkMap -> C.DocElement -> A.MarkdownElement
fromDocElement linkMap C.BlankLine             = A.BlankLine
fromDocElement linkMap C.ThematicBreak         = A.ThematicBreak
fromDocElement linkMap (C.Heading n xs)        = A.Heading n (parseInline linkMap $ B.intercalate "\n" xs)
fromDocElement linkMap (C.HeadingUnderline _)  = A.ThematicBreak
fromDocElement linkMap (C.IndentedBlock xs)    = A.Indented xs
fromDocElement linkMap (C.Fenced info xs)      = A.Fenced info xs
fromDocElement linkMap (C.ParagraphBlock xs)   = A.Paragraph (parseInline linkMap $ B.intercalate "\n" xs)
fromDocElement linkMap (C.LinkReference _ _ _) = A.BlankLine
fromDocElement linkMap (C.BlockquoteBlock xs)  = A.Blockquote $ map (fromDocElement linkMap) xs
fromDocElement linkMap (C.UListBlock xs)       = A.UnorderedList $ map (fromDoc linkMap) xs
fromDocElement linkMap (C.OListBlock xs)       = A.OrderedList $ map (second $ fromDoc linkMap) xs

-- Types

type BlockParser = StateT [Int] Parser
type InlineParser = StateT A.LinkMap Parser

indent :: Int -> BlockParser ()
indent n = modify ((:) n)

unindent :: BlockParser ()
unindent = modify (drop 1)

indentLevel :: BlockParser Int
indentLevel = liftM sum get

-- PARSING CST

parseDoc :: BlockParser C.Doc
parseDoc = sepBy (blankLine <|> block) (lift lineEnding)

block :: BlockParser C.DocElement
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

indented :: BlockParser C.DocElement -> BlockParser C.DocElement
indented p =
  indentLevel >>= \level ->
  optional (blankLine >> lift lineEnding) >> count level (lift whitespace) >> p

blankLine :: BlockParser C.DocElement
blankLine = lift $ next (lookAhead lineEnding_) *> return C.blankLine

headingUnderline :: BlockParser C.DocElement
headingUnderline = lift $ do
  optIndent
  (c:_) <- setextOf '=' <|> setextOf '-'
  return $ C.headingUnderline (if c == '=' then 1 else 2)
  where setextOf c = manyN 3 (char c) <* next (lookAhead lineEnding_)

thematicBreak :: BlockParser C.DocElement
thematicBreak = lift $ do
  optIndent
  thematicBreakOf '*' <|> thematicBreakOf '-' <|> thematicBreakOf '_'
  return C.thematicBreak
  where thematicBreakOf c = manyN 3 (next $ char c) <* next (lookAhead lineEnding_)

heading :: BlockParser C.DocElement
heading = lift $ do
  optIndent
  pounds <- atMostN1 6 (char8 '#')
  void whitespace <|> lookAhead lineEnding_
  content <- rawInline
  return $ C.heading (length pounds) content

indentedLine :: BlockParser C.DocElement
indentedLine = lift $ C.indentedBlock <$> (string "    " *> rawInline)

fenced :: BlockParser C.DocElement
fenced = do
  indentLen <- (+) <$> indentLevel <*> (length <$> lift optIndent)
  lift $ do
    fence <- fenceOf '`' <|> fenceOf '~'
    infoString <-  Atto.takeTill (\c -> c == '`' || isLineEnding c) <* lineEnding
    let closingFence = manyN (length fence) (char8 $ head fence)
        fenceIndent  = atMostN indentLen (char8 ' ')
    content <-  manyTill
                (fenceIndent *> Atto.takeTill isLineEnding <* lineEnding_)
                (endOfInput <|> (fenceIndent *> optIndent *> closingFence *> next (lookAhead lineEnding_)))
    return $ C.fenced infoString content
  where fenceOf c = manyN 3 (char c)

linkRef :: BlockParser C.DocElement
linkRef = lift $ do
  optIndent
  ref    <- linkReference <* char8 ':'
  dest   <- spacing *> linkDestination
  mtitle <- (optionMaybe $ spacing *> linkTitle) <* next (lookAhead lineEnding_)

  return $ C.linkReference ref dest mtitle
  where spacing = skipWhile isWhitespace >> (optional $ lineEnding *> skipMany1 whitespace)

paragraph :: BlockParser C.DocElement
paragraph = lift $ C.paragraphBlock <$> rawInline

blockquote :: BlockParser C.DocElement
blockquote = C.blockquoteBlock <$> (lift (optIndent *> char '>' *> skipWhile isWhitespace) *> block)

unorderedList :: BlockParser C.DocElement
unorderedList = do
  indent <- lift $ length <$> optIndent <* bulletMarker
  spaces <- lift $ B.length <$> takeWhile1 isWhitespace
  C.unorderedList <$> listItem (indent + 1 + spaces)

orderedList :: BlockParser C.DocElement
orderedList = do
  indent <- lift $ length <$> optIndent
  num <- lift orderedMarker
  spaces <- lift $ B.length <$> takeWhile1 isWhitespace
  C.orderedList (read num) <$> listItem (indent + length num + 1 + spaces)

listItem :: Int -> BlockParser C.Doc
listItem indentAmount =
  indent indentAmount *> ((:) <$> block <*> (many $ lift lineEnding >> indented block)) <* unindent

rawInline :: Parser B.ByteString
rawInline = Atto.takeTill isLineEnding

-- -- CST to AST and parsing inlines

type IgnoredChars = Maybe (S.Set Char)

isIgnored :: Char -> IgnoredChars -> Bool
isIgnored c Nothing = False
isIgnored c (Just set) = c `S.member` set

infixl 7 <+>
(<+>) :: IgnoredChars -> Char -> IgnoredChars
(<+>) Nothing c    = Just (S.insert c S.empty)
(<+>) (Just set) c = Just $ S.insert c set

lookupLink :: B.ByteString -> InlineParser (Maybe A.Link)
lookupLink ref = M.lookup (trim ref) <$> get

parseInline :: A.LinkMap -> B.ByteString -> A.MarkdownInline
parseInline linkMap x =
  case parse (inline Nothing) x of
    Left err   -> A.noInline
    Right val  -> fst val
  where parse p = parseOnly (runStateT p linkMap)

inline :: IgnoredChars -> InlineParser A.MarkdownInline
inline ignored =
  lift (endOfInput *> pure A.noInline)
  <|>
  (
    foldr (A.<#>) A.noInline
    <$> (many1 $ choice [
          codespan
          , em ignored
          , link ignored
          , image ignored
          , hardLineBreak
          , softLineBreak
          , escapedChar
          , inlineText ignored
        ])
  )

codespan :: InlineParser A.MarkdownInline
codespan = lift $ do
  open <- takeWhile1 (== '`')
  (A.codespan <$> codespanStr <* string open) <|> (pure $ A.text open)
  where codespanStr = scan [] scanf
        scanf ('\\':cs) c@'`' = Just (c:cs)
        scanf cs c = if c /= '`' then Just (c:cs) else Nothing

em :: IgnoredChars -> InlineParser A.MarkdownInline
em ignored = A.em <$> (flanked '*' '*' ignored <|> flanked '_' '_' ignored)
  where flanked :: Char -> Char -> IgnoredChars -> InlineParser A.MarkdownInline
        flanked open close ignored =
          lift (char open *> lookAhead printable)
          *> (inline $ ignored <+> close)
          <* lift (char close)

link :: IgnoredChars -> InlineParser A.MarkdownInline
link ignored = do
  linkText <- linkContent ignored
  (linkUrl, linkTitle) <- linkInfo
  case A.containsLink linkText of
    False -> return $ A.Link linkText linkUrl linkTitle
    _ -> fail "nested link"

image :: IgnoredChars-> InlineParser A.MarkdownInline
image ignored = lift (char '!') *> (uncurry <$> (A.image <$> linkContent ignored) <*> linkInfo)

linkContent :: IgnoredChars -> InlineParser A.MarkdownInline
linkContent ignored =
  lift (char '[') *> (inline $ ignored <+> ']') <* lift (char ']')

linkInfo :: InlineParser (B.ByteString, Maybe B.ByteString)
linkInfo = inlineLink <|> referenceLink
  where inlineLink = lift $ do
          dest <- char '(' *> skipWhile isWhitespace *> linkDestination
          title <- skipWhile isWhitespace *> optionMaybe linkTitle <* skipWhile isWhitespace <* char ')'
          return $ (dest, title)
        referenceLink = do
          linkRef <- lift linkReference
          mlink <- lookupLink linkRef
          case mlink of
            Nothing -> fail $ "Link reference: " ++ B.unpack linkRef ++ ", not found"
            Just (linkUrl, linkTitle) -> return (linkUrl, linkTitle)

hardLineBreak :: InlineParser A.MarkdownInline
hardLineBreak = lift $
  ((count 2 whitespace *> lineEnding_) <|> (char '\\' *> lineEnding_))
  *> pure A.hardLineBreak

softLineBreak :: InlineParser A.MarkdownInline
softLineBreak = lift $ lineEnding *> pure A.softLineBreak

escapedChar :: InlineParser A.MarkdownInline
escapedChar = lift $ A.text . ((flip B.cons) B.empty) <$> escaped
  where escaped = char '\\' *> satisfy isPunctuation

inlineText :: IgnoredChars -> InlineParser A.MarkdownInline
inlineText ignored = lift $ do
  x <- satisfy (\c -> not $ isIgnored c ignored)
  xs <- takeTill (flip elem special)
  return $ A.text (B.cons x xs)
  where special = "`*_[]!\n\\" :: String

-- -- USEFUL DEFINITIONS

lineEnding :: Parser ()
lineEnding = endOfLine

lineEnding_ :: Parser ()
lineEnding_ = endOfLine <|> endOfInput

whitespace :: Parser Char
whitespace = satisfy isWhitespace

optIndent :: Parser String
optIndent = atMostN 3 (char ' ')

printable :: Parser Char
printable = satisfy (not . isSpace)

punctuation :: Parser Char
punctuation = satisfy isPunctuation

control :: Parser Char
control = satisfy isControl

linkReference :: Parser B.ByteString
linkReference = between (char8 '[') (char8 ']') (Atto.takeWhile1 (\c -> c /= ']'))

linkDestination :: Parser B.ByteString
linkDestination = Atto.takeWhile1 (\c -> not $ isSpace c || isControl c || c == '(' || c == ')')

linkTitle :: Parser B.ByteString
linkTitle =  titleOf '"' <|> titleOf '\'' <|> Atto.takeWhile1 (\c -> isPrintable c && c /= '(' && c /= ')')
  where titleOf x = char x *> Atto.takeWhile (\c -> c /= x) <* char x

bulletMarker :: Parser Char
bulletMarker = oneOf "-+*"

orderedMarker :: Parser String
orderedMarker = atMostN1 10 digit <* oneOf ".)"

next :: Parser a -> Parser a
next p = skipWhile isWhitespace *> p

-- USEFUL COMBINATORS

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (\c -> c `elem` cs)

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing $ Just <$> p

manyN :: Int -> Parser a -> Parser [a]
manyN n p
  | n <= 0 = return []
  | otherwise = liftM2 (++) (count n p) (many p)

atMostN :: Int -> Parser a -> Parser [a]
atMostN n p = atMostN1 n p <|> return []

atMostN1 :: Int -> Parser a -> Parser [a]
atMostN1 n p
  | n <= 1 = count 1 p
  | otherwise = count n p <|> atMostN1 (n - 1) p

-- USEFUL FUNCTIONS

isWhitespace :: Char -> Bool
isWhitespace c = isSpace c && c /= '\n'

isLineEnding :: Char -> Bool
isLineEnding c = c == '\n' || c == '\r'

isPrintable :: Char -> Bool
isPrintable = not . isSpace
