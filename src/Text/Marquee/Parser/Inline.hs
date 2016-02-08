{-# LANGUAGE OverloadedStrings #-}

module Text.Marquee.Parser.Inline where

import Control.Applicative
import Control.Monad
import Control.Monad.State (StateT(..), get, modify, lift)

import Data.Char (isAlphaNum, isControl, isDigit, isLetter, isPunctuation, isSymbol)
import Data.Text (Text())
import qualified Data.Text as T
import qualified Data.Map as M (lookup)
import qualified Data.Set as S

import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator

import Data.Text.Marquee (trim)
import Text.Marquee.Parser.Common
import Text.Marquee.Parser.HTML
import Text.Marquee.SyntaxTrees.AST (MarkdownInline(..))
import qualified Text.Marquee.SyntaxTrees.AST as A

-- Types

type InlineParser = StateT A.LinkMap Parser

lookupLink :: Text -> InlineParser (Maybe A.Link)
lookupLink ref = M.lookup (T.toLower . trim $ ref) <$> get

type IgnoredChars = Maybe (S.Set Char)

isIgnored :: Char -> IgnoredChars -> Bool
isIgnored c Nothing = False
isIgnored c (Just set) = c `S.member` set

infixl 7 <+>
(<+>) :: IgnoredChars -> Char -> IgnoredChars
(<+>) Nothing c    = Just (S.insert c S.empty)
(<+>) (Just set) c = Just $ S.insert c set

-- Definitions

-- Parsing

parseInline :: A.LinkMap -> Text -> MarkdownInline
parseInline linkMap x =
  case parse (inline Nothing) x of
    Left err   -> A.noInline
    Right val  -> fst val
  where parse p = parseOnly (runStateT p linkMap)

inline :: IgnoredChars -> InlineParser MarkdownInline
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
          , autolink
          , html
          , hardLineBreak
          , softLineBreak
          , escapedChar
          , inlineText ignored
        ])
  )

codespan :: InlineParser MarkdownInline
codespan = lift $ do
  open <- Atto.takeWhile1 (== '`')
  let noCodespan = return . A.text $ open
      closing = string open >> lookAhead (void (notChar '`') <|> endOfInput)
      codespan' = A.codespan . T.concat <$> manyTill codespanStr closing
      codespanStr = Atto.takeWhile1 (/= '`')
                    <|> (Atto.takeWhile1 (== '`') >>= \ticks ->
                        if T.length ticks == T.length open  then fail "Found closing codespan"
                                                            else return ticks)
  codespan' <|> noCodespan

em :: IgnoredChars -> InlineParser MarkdownInline
em ignored = A.em <$> (flanked '*' '*' ignored <|> flanked '_' '_' ignored)
  where flanked :: Char -> Char -> IgnoredChars -> InlineParser MarkdownInline
        flanked open close ignored =
          lift (char open *> lookAhead printable)
          *> (inline $ ignored <+> close)
          <* lift (char close)

link :: IgnoredChars -> InlineParser MarkdownInline
link ignored = fullLink <|> simpleLink
  where fullLink = do
          linkText <- linkContent ignored
          (_, linkUrl, linkTitle) <- linkInfo
          case A.containsLink linkText of
            False -> return $ A.link linkText linkUrl linkTitle
            _ -> fail "nested link"
        simpleLink = do
          (linkRef, linkUrl, linkTitle) <- linkRefInfo
          return $ A.link (A.text linkRef) linkUrl linkTitle

image :: IgnoredChars-> InlineParser MarkdownInline
image ignored = do
  lift (char '!')
  Link content url title <- link ignored
  return $ A.image content url title

linkContent :: IgnoredChars -> InlineParser MarkdownInline
linkContent ignored =
  lift (char '[') *> (inline $ ignored <+> ']') <* lift (char ']')

linkInfo :: InlineParser (Text, Text, Maybe Text)
linkInfo = linkInlineInfo <|> linkRefInfo

linkInlineInfo :: InlineParser (Text, Text, Maybe Text)
linkInlineInfo = lift $ do
  dest <- char '(' *> skipWhile isLinespace *> linkDestination
  title <- skipWhile isLinespace *> optionMaybe linkTitle <* skipWhile isLinespace <* char ')'
  return $ (T.empty, dest, title)

linkRefInfo :: InlineParser (Text, Text, Maybe Text)
linkRefInfo = do
  linkRef <- lift linkLabel
  mlink <- lookupLink linkRef
  case mlink of
    Nothing -> fail $ "Link reference: " ++ T.unpack linkRef ++ ", not found"
    Just (linkUrl, linkTitle) -> return (linkRef, linkUrl, linkTitle)

-- TODO: Write valid email address parser
autolink :: InlineParser MarkdownInline
autolink = lift $ do
  char '<'
  (url, label) <- absoluteUri <|> emailUri
  char '>'
  return $ A.link (A.text label) url Nothing
  where absoluteUri = do
          xs <- scheme
          y <- char ':'
          ys <- Atto.takeWhile (\c -> not $ isWhitespace c || isControl c || c `elem` ("<>" :: String))
          let url = T.append xs (T.cons y ys)
          return (url, url)
        scheme = do
          x <- satisfy isLetter
          xs <- Atto.takeWhile1 (\c -> isLetter c || isDigit c || c `elem` ("+.-" :: String))
          return $ T.cons x xs
        emailUri = do
          stitch
            <$> takeWhile1 (\c -> not $ c `elem` ("@<>" :: String))
            <*> char '@'
            <*> takeWhile1 (\c -> not $ c `elem` ("@<>" :: String))
        stitch local at server =
          let email = T.concat [local, T.singleton at, server]
          in (T.append "mailto:" email, email)

html :: InlineParser MarkdownInline
html = lift $ do
  tag <- tag <|> ctag
  return $ A.htmlText tag

hardLineBreak :: InlineParser MarkdownInline
hardLineBreak = lift $
  ((manyN 2 linespace *> lineEnding) <|> (char '\\' *> lineEnding)) *> pure A.hardLineBreak

softLineBreak :: InlineParser MarkdownInline
softLineBreak = lift $ lineEnding *> pure A.softLineBreak

escapedChar :: InlineParser MarkdownInline
escapedChar = lift $ A.text . ((flip T.cons) T.empty) <$> escaped
  where escaped = char '\\' *> satisfy (\c -> isPunctuation c || isSymbol c)

inlineText :: IgnoredChars -> InlineParser MarkdownInline
inlineText ignored = lift $ do
  x <- satisfy (\c -> not $ isIgnored c ignored)
  xs <- manyTill anyChar
        (lookAhead $ endOfInput <|> void (oneOf special) <|> manyN 2 linespace *> lineEnding)
  return $ A.text . T.pack $ x:xs
  where special = "`*_[]!\n\\<" :: String

