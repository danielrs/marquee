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

type BlockParser = Parsec String [Int]

indent :: Int -> BlockParser Int
indent n = modifyState ((:) n) >> indentLevel

unindent :: BlockParser ()
unindent = modifyState f
  where f []     = []
        f (_:xs) = xs

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
  , try indentedBlock
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
blankLine = skipMany whitespace >> lookAhead lineEnding_ >> return C.blankLine

blankLine2 :: BlockParser ()
blankLine2 = blankLine >> lineEnding >> blankLine >> return ()

headingUnderline :: BlockParser C.DocElement
headingUnderline = do
  optIndent
  c <- try (setextOf '=') <|> setextOf '-'
  return $ C.headingUnderline (if c == '=' then 1 else 2)
  where setextOf c = manyN 3 (char c) >>= \(c:_) -> skipMany whitespace >> lookAhead lineEnding_ >> return c

thematicBreak :: BlockParser C.DocElement
thematicBreak = do
  optIndent
  try (thematicBreakOf '*') <|> try (thematicBreakOf '-') <|> thematicBreakOf '_'
  return C.thematicBreak
  where thematicBreakOf c = manyN 3 (char c >> skipMany whitespace) >> lookAhead lineEnding_

heading :: BlockParser C.DocElement
heading = do
  optIndent
  pounds <- atMostN1 6 (char '#')
  (whitespace >> return ()) <|> lookAhead lineEnding_
  content <- rawInline
  return $ C.heading (length pounds) content

indentedBlock :: BlockParser C.DocElement
indentedBlock = do
  count 4 (char ' ')
  content <- rawInline
  return $ C.indentedBlock content

fenced :: BlockParser C.DocElement
fenced = do
  indentLen <- indentLevel >>= \level -> optIndent >>= \opt -> return (level + length opt)

  fence <- try (fenceOf '`') <|> fenceOf '~'
  let fenceLen  = length fence
      fenceChar = char $ head fence
      fenceIndent = atMostN indentLen (char ' ')

  infoString <- manyTill infoChar lineEnding_
  content    <- manyTill
                (fenceIndent >> manyTill anyChar lineEnding_)
                (try (fenceIndent >> optIndent >> manyN fenceLen fenceChar >> lookAhead lineEnding_) <|> eof)

  return $ C.fenced infoString content
  where fenceOf c    = manyN 3 (char c)
        infoChar     = notFollowedBy (char '`') >> anyChar

linkRef :: BlockParser C.DocElement
linkRef = do
  optIndent
  reference <- between (char '[') (char ']') (skipMany whitespace >> linkReference)
  char ':'

  skipMany whitespace >> (optional $ lineEnding >> skipMany1 whitespace)
  destination <- many1 printable
  skipMany whitespace
  title <- option "" $ parseTitle <|> try (lineEnding_ >> skipMany1 whitespace >> parseTitle)

  return $ C.linkReference
            reference
            destination
            (if length title > 0 then Just title else Nothing)
  where parseTitle = printable >>= \t -> manyTill anyChar (lookAhead lineEnding_) >>= return . (:) t

paragraph :: BlockParser C.DocElement
paragraph = do
  content <- rawInline
  return $ C.paragraphBlock content

blockquote :: BlockParser C.DocElement
blockquote = do
  optIndent
  char '>' >> optional whitespace
  skipMany whitespace
  content <- block
  return $ C.blockquoteBlock content

unorderedList :: BlockParser C.DocElement
unorderedList = do
  optIndent

  bulletMarker
  spaces <- many1 whitespace

  indent (1 + length spaces)
  x <- block
  xs <- many $ try $ lineEnding >> indented block
  unindent

  return $ C.unorderedList (x:xs)

orderedList :: BlockParser C.DocElement
orderedList = do
  optIndent

  num <- orderedMarker
  spaces <- many1 whitespace

  indent (length num + 1 + length spaces)
  x <- block
  xs <- many $ try $ lineEnding >> indented block
  unindent

  return $ C.orderedList (read num) (x:xs)

rawInline :: BlockParser String
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

lookupLink :: String -> InlineParser (Maybe A.Link)
lookupLink ref = getState >>= return . M.lookup ref

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
                          , try $ image rules
                          , try hardLineBreak
                          , try escapedChar
                          , try $ inlineText rules]
    return $ foldr (A.<#>) A.noInline xs
  )

codespan :: InlineParser A.MarkdownInline
codespan = do
  open <- many1 (char '`')
  try (A.codespan <$> many codespanChar <* count (length open) (char $ head open)) <|> (return $ A.text open)
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
          linkRef <- liftM trim $ skipMany whitespace >> linkReference
          char ']'
          mlink <- lookupLink (map toLower . trim $ linkRef)
          case mlink of
            Nothing -> fail $ "Link reference: " ++ linkRef ++ ", not found"
            Just (linkUrl, linkTitle) -> return (linkUrl, linkTitle)

image :: ParsingRules-> InlineParser A.MarkdownInline
image rules = do
  char '!'
  linkText <- linkContent rules
  (linkUrl, linkTitle) <- linkInfo
  return $ A.image linkText linkUrl linkTitle

hardLineBreak :: InlineParser A.MarkdownInline
hardLineBreak = do
  try (manyN 2 whitespace >> lookAhead lineEnding_) <|> (char '\\' >> lookAhead lineEnding_)
  return A.HardLineBreak

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

escapedChar :: InlineParser A.MarkdownInline
escapedChar =  (notFollowedBy lineEnding >> escaped) >>= return . A.text . (:[])
  where escaped :: InlineParser Char
        escaped = char '\\' >> satisfy isPunctuation

inlineText :: ParsingRules -> InlineParser A.MarkdownInline
inlineText rules =
  manyTill1
  (runRules rules >> anyChar)
  (lineEnding_ <|> (void $ lookAhead $ oneOf "`*_[]\\"))
  >>= return . A.text

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
