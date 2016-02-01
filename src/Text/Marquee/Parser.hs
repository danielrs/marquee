module Text.Marquee.Parser (parseDoc, renderCST, renderAST) where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Reader

import Data.Char (isControl, isPunctuation, isSpace, toLower)
import Data.Either (rights)
import Data.List (intercalate)
import qualified Data.Map as M (lookup)

import Text.Parsec (Parsec(..), modifyState)
import Text.ParserCombinators.Parsec hiding (spaces, space)
import Text.ParserCombinators.Parsec.Char (oneOf, noneOf)

import Data.String.Marquee
import Text.ParserCombinators.Parsec.Marquee

import qualified Text.Marquee.SyntaxTrees.CST as C
import qualified Text.Marquee.SyntaxTrees.AST as A

-- API

renderCST :: String -> C.Doc
renderCST input = case parse parseDoc "Markdown" input of
                    Left err -> error $ show err
                    Right val -> C.clean $ val
  where parse p = runParser p []

renderAST :: String -> A.Markdown
renderAST input =
  case parse parseDoc "CST-parse" input of
    Left err -> error . show $ err
    Right cst ->
      let (linkMap, cst') = second C.clean . A.stripLinkReferences $ cst
      in map (fromDocElement linkMap) cst'
  where parse p = runParser p []


-- PARSING CST

-- Block parser where the state is indent levels in a stack
type BlockParser = Parsec String [Int]

indent :: Int -> BlockParser Int
indent n = modifyState ((:) n) >> indentLevel

unindent :: BlockParser ()
unindent = modifyState (drop 1)

indentLevel :: BlockParser Int
indentLevel = liftM sum getState

parseDoc :: BlockParser C.Doc
parseDoc = sepEndBy block lineEnding

block :: BlockParser C.DocElement
block = choice [
  try blankLine
  , try headingUnderline
  , try thematicBreak
  , try heading
  , try indentedLine
  , try fenced
  , try linkRef
  -- Container
  , try blockquote
  , try unorderedList
  , try orderedList
  -- Any other is a paragraph
  , paragraph
  ]

indented :: BlockParser C.DocElement -> BlockParser C.DocElement
indented p =
  indentLevel >>= \level ->
  try (count level whitespace >> p) <|> (notFollowedBy blankLine2 >> blankLine)

blankLine :: BlockParser C.DocElement
blankLine = skipMany whitespace *> lookAhead lineEnding_ *> return C.blankLine

blankLine2 :: BlockParser ()
blankLine2 = blankLine *> lineEnding *> blankLine *> pure ()

headingUnderline :: BlockParser C.DocElement
headingUnderline = do
  optIndent
  (c:_) <- try (setextOf '=') <|> setextOf '-'
  return $ C.headingUnderline (if c == '=' then 1 else 2)
  where setextOf c = manyN 3 (char c) <* skipMany whitespace <* lookAhead lineEnding_

thematicBreak :: BlockParser C.DocElement
thematicBreak = do
  optIndent
  try (thematicBreakOf '*') <|> try (thematicBreakOf '-') <|> thematicBreakOf '_'
  return C.thematicBreak
  where thematicBreakOf c = manyN 3 (char c <* skipMany whitespace) <* lookAhead lineEnding_

heading :: BlockParser C.DocElement
heading = do
  optIndent
  pounds <- atMostN1 6 (char '#')
  (whitespace *> pure ()) <|> lookAhead lineEnding_
  content <- rawInline
  return $ C.heading (length pounds) content

indentedLine :: BlockParser C.DocElement
indentedLine = C.indentedBlock <$> (count 4 (char ' ') *> rawInline)

fenced :: BlockParser C.DocElement
fenced = do
  indentLen <- (+) <$> indentLevel <*> (length <$> optIndent)

  fence <- try (fenceOf '`') <|> fenceOf '~'
  let closingFence = manyN (length fence) (char $ head fence)
      fenceIndent  = atMostN indentLen (char ' ')

  infoString <- manyTill infoChar lineEnding_
  content    <- manyTill
                (fenceIndent *> manyTill anyChar lineEnding_)
                (eof <|> try (fenceIndent *> optIndent *> closingFence *> lookAhead lineEnding_))

  return $ C.fenced infoString content
  where fenceOf c = manyN 3 (char c)
        infoChar  = notFollowedBy (char '`') *> anyChar

linkRef :: BlockParser C.DocElement
linkRef = do
  optIndent
  ref <- between (char '[') (char ']') (skipMany whitespace *> linkReference)
  char ':'

  skipMany whitespace >> (optional $ lineEnding *> skipMany1 whitespace)
  dest <- many1 printable
  skipMany whitespace
  mtitle <- optionMaybe $ parseTitle <|> try (lineEnding *> skipMany1 whitespace *> parseTitle)

  return $ C.linkReference ref dest mtitle
  where parseTitle = (:) <$> printable <*> manyTill anyChar (lookAhead lineEnding_)

paragraph :: BlockParser C.DocElement
paragraph = C.paragraphBlock <$> rawInline

blockquote :: BlockParser C.DocElement
blockquote = C.blockquoteBlock <$> (optIndent *> char '>' *> skipMany whitespace *> block)

unorderedList :: BlockParser C.DocElement
unorderedList = do
  optIndent
  bulletMarker
  spaces <- many1 whitespace
  C.unorderedList <$> listItem (1 + length spaces)

orderedList :: BlockParser C.DocElement
orderedList = do
  optIndent
  num <- orderedMarker
  spaces <- many1 whitespace
  C.orderedList (read num) <$> listItem (length num + 1 + length spaces)

listItem :: Int -> BlockParser C.Doc
listItem indentAmount =
  indent indentAmount *> ((:) <$> block <*> (many $ try $ lineEnding >> indented block)) <* unindent

rawInline :: BlockParser String
rawInline = manyTill anyChar (lookAhead lineEnding_)

-- CST to AST and parsing inlines

type ParsingRules = Maybe (InlineParser ())

runRules :: ParsingRules -> InlineParser ()
runRules (Nothing)    = return ()
runRules (Just rules) = rules

infixl 7 <+>
(<+>) :: ParsingRules -> InlineParser () -> ParsingRules
(<+>) Nothing rule      = Just rule
(<+>) (Just rules) rule = Just $ rules *> rule

type InlineParser = Parsec String A.LinkMap

lookupLink :: String -> InlineParser (Maybe A.Link)
lookupLink ref = M.lookup ref <$> getState

parseInlines :: A.LinkMap -> [String] -> A.MarkdownInline
parseInlines linkMap xs =
  case sequence $ map (parse (inline Nothing) "markdown-inline") xs of
    Left err -> error . show $ err
    Right val -> foldr (A.</>) A.noInline val
  where parse p = runParser p linkMap

inline :: ParsingRules -> InlineParser A.MarkdownInline
inline rules =
  (eof *> pure A.noInline)
  <|>
  (
    foldr (A.<#>) A.noInline
    <$> (many1 $ choice [
          try codespan
          , try $ bold rules
          , try $ italic rules
          , try $ link rules
          , try $ image rules
          , try hardLineBreak
          , try escapedChar
          , try $ inlineText rules
        ])
  )

codespan :: InlineParser A.MarkdownInline
codespan = do
  open <- many1 (char '`')
  try (A.codespan <$> many codespanChar <* count (length open) (char $ head open)) <|> (return $ A.text open)
  where codespanChar = (string "\\`" >> return '`') <|> noneOf "`"

bold :: ParsingRules -> InlineParser A.MarkdownInline
bold rules = A.bold <$> (try (flanked "**" "**" rules) <|> flanked "__" "__" rules)

italic :: ParsingRules -> InlineParser A.MarkdownInline
italic rules = A.italic <$> (try (flanked "*" "*" rules) <|> flanked "_" "_" rules)

flanked :: String -> String -> ParsingRules -> InlineParser A.MarkdownInline
flanked open close rules =
  string open *> notFollowedBy whitespace
  *> (inline $ rules <+> notFollowedBy (string close)) <* string close

link :: ParsingRules -> InlineParser A.MarkdownInline
link rules = do
  linkText <- linkContent rules
  (linkUrl, linkTitle) <- linkInfo
  case A.containsLink linkText of
    False -> return $ A.Link linkText linkUrl linkTitle
    _ -> unexpected "nested link"

linkContent :: ParsingRules -> InlineParser A.MarkdownInline
linkContent rules = char '[' *> (inline $ rules <+> notFollowedBy (char ']')) <* char ']'

linkInfo :: InlineParser (String, Maybe String)
linkInfo = inlineLink <|> referenceLink
  where inlineLink =
          char '('
          *> ((,) <$> linkDestination <*> (skipMany whitespace *> try (optionMaybe linkTitle)))
          <* skipMany whitespace <* char ')'
        referenceLink = do
          char '['
          linkRef <- liftM trim $ skipMany whitespace *> linkReference
          char ']'
          mlink <- lookupLink (map toLower . trim $ linkRef)
          case mlink of
            Nothing -> fail $ "Link reference: " ++ linkRef ++ ", not found"
            Just (linkUrl, linkTitle) -> return (linkUrl, linkTitle)

image :: ParsingRules-> InlineParser A.MarkdownInline
image rules = char '!' *> (uncurry <$> (A.image <$> linkContent rules) <*> linkInfo)

hardLineBreak :: InlineParser A.MarkdownInline
hardLineBreak =
  (try (manyN 2 whitespace *> lookAhead lineEnding_) <|> (char '\\' *> lookAhead lineEnding_))
  *> pure A.hardLineBreak

escapedChar :: InlineParser A.MarkdownInline
escapedChar = A.text . (:[]) <$> (notFollowedBy lineEnding *> escaped)
  where escaped :: InlineParser Char
        escaped = char '\\' *> satisfy isPunctuation

inlineText :: ParsingRules -> InlineParser A.MarkdownInline
inlineText rules =
  A.text <$>
  manyTill1
  (runRules rules *> anyChar)
  (lineEnding_ <|> (void $ lookAhead $ oneOf "`*_[]\\"))

fromDoc :: A.LinkMap -> C.Doc -> A.Markdown
fromDoc linkMap = map (fromDocElement linkMap)

fromDocElement :: A.LinkMap -> C.DocElement -> A.MarkdownElement
fromDocElement linkMap C.BlankLine             = A.BlankLine
fromDocElement linkMap C.ThematicBreak         = A.ThematicBreak
fromDocElement linkMap (C.Heading n xs)        = A.Heading n (parseInlines linkMap xs)
fromDocElement linkMap (C.HeadingUnderline _)  = A.ThematicBreak
fromDocElement linkMap (C.IndentedBlock xs)    = A.Indented xs
fromDocElement linkMap (C.Fenced info xs)      = A.Fenced info xs
fromDocElement linkMap (C.ParagraphBlock xs)   = A.Paragraph (parseInlines linkMap xs)
fromDocElement linkMap (C.LinkReference _ _ _) = A.BlankLine
fromDocElement linkMap (C.BlockquoteBlock xs)  = A.Blockquote $ map (fromDocElement linkMap) xs
fromDocElement linkMap (C.UListBlock xs)       = A.UnorderedList $ map (fromDoc linkMap) xs
fromDocElement linkMap (C.OListBlock xs)       = A.OrderedList $ map (second $ fromDoc linkMap) xs

-- USEFUL DEFINITIONS

lineEnding :: Parsec String u String
lineEnding = (string "\r\n" <|> string "\n" <|> string "\r") >>= return

lineEnding_ :: Parsec String u ()
lineEnding_ = (lineEnding >> return ()) <|> eof

whitespace :: Parsec String u Char
whitespace = satisfy (\c -> isSpace c && c /= '\n')

optIndent :: Parsec String u [Char]
optIndent = atMostN 3 (char ' ')

printable :: Parsec String u Char
printable = satisfy (not . isSpace)

punctuation :: Parsec String u Char
punctuation = satisfy isPunctuation

control :: Parsec String u Char
control = satisfy isControl

linkReference :: Parsec String u String
linkReference = many1 $ (string "\\]" >> return ']') <|> noneOf "]"

linkDestination :: Parsec String u String
linkDestination = manyTill anyChar (lookAhead $ choice [whitespace, control, char '(', char ')'])

linkTitle :: Parsec String u String
linkTitle = try (titleOf '"') <|> titleOf '\''
  where titleOf :: Char -> Parsec String u String
        titleOf c = char c >> manyTill anyChar (char c)

bulletMarker :: Parsec String u Char
bulletMarker = oneOf "-+*"

orderedMarker :: Parsec String u String
orderedMarker = atMostN1 10 digit <* oneOf ".)"
