module Text.Marquee.Parser (parseDoc, renderCST, renderAST) where

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Reader

import Data.Char (isControl, isPunctuation, isSpace)
import Data.Either (rights)
import Data.List (intercalate)

import Text.Parsec (Parsec(..))
import Text.ParserCombinators.Parsec hiding (spaces, space)
import Text.ParserCombinators.Parsec.Char (oneOf, noneOf)

import Data.String.Marquee
import Text.ParserCombinators.Parsec.Marquee

import qualified Text.Marquee.CST as C
import qualified Text.Marquee.AST as A

-- API

renderCST :: String -> C.Doc
renderCST input = case parse parseDoc "Markdown" input of
                    Left err -> error $ show err
                    Right val -> C.clean $ val

renderAST :: String -> A.Markdown
renderAST input =
  case parse parseDoc "CST-parse" input of
    Left err -> error . show $ err
    Right cst ->
      let (linkMap, cst') = A.stripLinkReferences . C.clean $ cst
      in map (fromDocElement linkMap) cst'


-- PARSING CST

parseDoc :: Parser C.Doc
parseDoc = sepEndBy block lineEnding

block :: Parser C.DocElement
block = choice [
  try blankLine
  , try headingUnderline
  , try thematicBreak
  , try heading
  , try indented
  , try fenced
  , try linkReference
  -- Container
  , try blockquote
  , try unorderedList
  , try orderedList
  -- Any other is a paragraph
  , paragraph
  ]

blankLine :: Parser C.DocElement
blankLine = do
  manyTill whitespace (lookAhead lineEnding_)
  return C.blankLine

headingUnderline :: Parser C.DocElement
headingUnderline = do
  indent
  c <- try (setextOf '=') <|> setextOf '-'
  return $ C.headingUnderline (if c == '=' then 1 else 2)
  where setextOf c = manyN 3 (char c) >>= \(c:_) -> skipMany whitespace >> lookAhead lineEnding_ >> return c

thematicBreak :: Parser C.DocElement
thematicBreak = do
  indent
  try (thematicBreakOf '*') <|> try (thematicBreakOf '-') <|> thematicBreakOf '_'
  return C.thematicBreak
  where thematicBreakOf c = manyN 3 (char c >> skipMany whitespace) >> lookAhead lineEnding_

heading :: Parser C.DocElement
heading = do
  indent
  pounds <- atMostN1 6 (char '#')
  (whitespace >> return ()) <|> lookAhead lineEnding_
  content <- rawInline
  return $ C.heading (length pounds) content

indented :: Parser C.DocElement
indented = do
  count 4 (char ' ')
  content <- rawInline
  return $ C.indentedBlock content

fenced :: Parser C.DocElement
fenced = do
  indentLen <- indent >>= return . length

  fence <- try (fenceOf '`') <|> fenceOf '~'
  let fenceLen  = length fence
      fenceChar = char $ head fence
      fenceIndent = atMostN indentLen (char ' ')

  infoString <- manyTill infoChar lineEnding_
  content    <- manyTill
                (fenceIndent >> manyTill anyChar lineEnding_)
                (try (fenceIndent >> indent >> manyN fenceLen fenceChar >> lookAhead lineEnding_) <|> eof)

  return $ C.fenced infoString content
  where fenceOf c    = manyN 3 (char c)
        infoChar     = notFollowedBy (char '`') >> anyChar

linkReference :: Parser C.DocElement
linkReference = do
  indent
  reference <- between (char '[') (char ']') (skipMany whitespace >> many1 linkTextChar)
  char ':'

  skipMany whitespace >> (optional $ lineEnding >> skipMany1 whitespace)
  destination <- many1 printable

  skipMany whitespace
  title <- option "" $ parseTitle <|> (lineEnding_ >> skipMany1 whitespace >> parseTitle)

  return $ C.linkReference
            reference
            destination
            (if length title > 0 then Just title else Nothing)
  where parseTitle = printable >>= \t -> manyTill anyChar (lookAhead lineEnding_) >>= return . (:) t

paragraph :: Parser C.DocElement
paragraph = do
  content <- rawInline
  return $ C.paragraphBlock content

blockquote :: Parser C.DocElement
blockquote = do
  indent
  char '>' >> optional whitespace
  skipMany whitespace
  content <- block
  return $ C.blockquoteBlock content

unorderedList :: Parser C.DocElement
unorderedList = do
  indent
  bulletMarker >> whitespace
  content <- block
  return $ C.unorderedList content

orderedList :: Parser C.DocElement
orderedList = do
  indent
  num <- orderedMarker >>= \x -> whitespace >> return x
  content <- block
  return $ C.orderedList num content

rawInline :: Parser String
rawInline = manyTill anyChar (lookAhead lineEnding_)

-- CST to AST and parsing inlines

type ParsingRules = Maybe (InlineParser ())

runRules :: ParsingRules -> InlineParser ()
runRules (Nothing)    = return ()
runRules (Just rules) = rules

infixl 7 >>+
(>>+) :: ParsingRules -> InlineParser () -> ParsingRules
(>>+) Nothing rule      = Just rule
(>>+) (Just rules) rule = Just $ rules >> rule

type InlineParser = Parsec String A.LinkMap

parseInlines :: A.LinkMap -> [String] -> A.MarkdownInline
parseInlines linkMap xs =
  case sequence $ map (parse (inline Nothing) "markdown-inline") xs of
    Left err -> error . show $ err
    Right val -> foldr (A.</>) A.noInline val
  where parse p = runParser p linkMap

inline :: ParsingRules -> InlineParser A.MarkdownInline
inline rules =
  (eof >> return A.noInline)
  <|>
  (do
    xs <- many1 $ choice  [try codespan
                          , try $ bold rules
                          , try $ italic rules
                          , try $ link rules
                          -- , try $ matchingInline
                          , inlineText rules]
    return $ foldr (A.<#>) A.noInline xs
  )

codespan :: InlineParser A.MarkdownInline
codespan = do
  open <- many1 (char '`')
  content <- many codespanChar
  count (length open) (char $ head open)
  return $ A.codespan content
  where codespanChar = (string "\\`" >> return '`') <|> noneOf "`"

bold :: ParsingRules -> InlineParser A.MarkdownInline
bold rules = do
  xs <- try (flanked "**" "**" rules)
        <|> flanked "__" "__" rules
  return $ A.bold xs

italic :: ParsingRules -> InlineParser A.MarkdownInline
italic rules = do
  xs <- try (flanked "*" "*" rules)
        <|> flanked "_" "_" rules
  return $ A.italic xs

flanked :: String -> String -> ParsingRules -> InlineParser A.MarkdownInline
flanked open close rules = do
  string open
  notFollowedBy whitespace
  content <- inline $ rules >>+ notFollowedBy (string close)
  string close
  return $ content

link :: ParsingRules -> InlineParser A.MarkdownInline
link rules = do
  linkText <- linkContent rules
  (linkUrl, linkTitle) <- linkInfo
  case A.containsLink linkText of
    False -> return $ A.Link linkText linkUrl linkTitle
    _ -> unexpected "nested link"

linkContent :: ParsingRules -> InlineParser A.MarkdownInline
linkContent rules = do
  char '['
  content <- inline $ rules >>+ notFollowedBy (char ']')
  char ']'
  return $ content

linkInfo :: InlineParser (String, Maybe String)
linkInfo = inlineLink <|> referenceLink
  where inlineLink    = do
          char '('
          linkUrl <- linkDestination
          linkTitle <- skipMany whitespace >> try (optionMaybe linkTitle)
          skipMany whitespace
          char ')'
          return (linkUrl, linkTitle)
        referenceLink = do
          char '['
          char ']'
          return ("ref.com", Nothing)


image :: InlineParser A.MarkdownInline
image = undefined

-- matchingInline :: InlineParser A.MarkdownInline
-- matchingInline = do
--   match <- matching
--   return $ A.text match

-- matching :: Parsec String u String
-- matching = do
--   open <- char '(' <|> char '[' <|> char '{'
--   cs <- matching' open
--   return (open:cs)
--   where closing '(' = ')'
--         closing '[' = ']'
--         closing '{' = '}'
--         matching' :: Char -> Parsec String u String
--         matching' open = (char (closing open) >>= return . (:[]))
--                     <|> (anyChar >>= \c -> matching' open >>= \cs -> return (c:cs))

inlineText :: ParsingRules -> InlineParser A.MarkdownInline
inlineText rules =
  manyTill1
  (runRules rules >> (escapedChar <|> anyChar))
  (lineEnding_ <|> (void $ lookAhead $ oneOf "*_[]"))
  >>= return . A.text

escapedChar :: InlineParser Char
escapedChar =  ((notFollowedBy lineEnding >> escaped) <|> anyChar) >>= return
  where escaped :: InlineParser Char
        escaped = char '\\' >> satisfy isPunctuation

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
fromDocElement linkMap (C.UListBlock xs)       = A.UnorderedList $ map (fromDocElement linkMap) xs
fromDocElement linkMap (C.OListBlock xs)       = A.OrderedList $ map (second $ fromDocElement linkMap) xs

-- USEFUL DEFINITIONS

lineEnding :: Parsec String u String
lineEnding = (string "\r\n" <|> string "\n" <|> string "\r") >>= return

lineEnding_ :: Parsec String u ()
lineEnding_ = (lineEnding >> return ()) <|> eof

whitespace :: Parsec String u Char
whitespace = satisfy (\c -> isSpace c && c /= '\n')

indent :: Parsec String u [Char]
indent = atMostN 3 (char ' ')

printable :: Parsec String u Char
printable = satisfy (not . isSpace)

punctuation :: Parsec String u Char
punctuation = satisfy isPunctuation

control :: Parsec String u Char
control = satisfy isControl

linkTextChar :: Parsec String u Char
linkTextChar = (string "\\]" >> return ']') <|> noneOf "]"

linkDestination :: Parsec String u String
linkDestination = manyTill anyChar (lookAhead $ choice [whitespace, control, char '(', char ')'])

linkTitle :: Parsec String u String
linkTitle = try (titleOf '"') <|> titleOf '\''
  where titleOf :: Char -> Parsec String u String
        titleOf c = char c >> manyTill anyChar (char c)

bulletMarker :: Parser Char
bulletMarker = oneOf "-+*"

orderedMarker :: Parser Int
orderedMarker = atMostN1 10 digit >>= return . read >>= \x -> oneOf ".)" >> return x
