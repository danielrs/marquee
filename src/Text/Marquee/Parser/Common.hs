{-# LANGUAGE OverloadedStrings #-}

module Text.Marquee.Parser.Common where

import Control.Applicative
import Control.Monad

import Data.Char (isControl, isPunctuation, isSpace, isSymbol)
import Data.Text (Text())
import qualified Data.Text as T

import Data.Attoparsec.Text as Atto
import Data.Attoparsec.Combinator

-- Useful parsers

lineEnding :: Parser ()
lineEnding = endOfLine

lineEnding_ :: Parser ()
lineEnding_ = endOfLine <|> endOfInput

whitespace :: Parser Char
whitespace = satisfy isWhitespace

linespace :: Parser Char
linespace = satisfy isLinespace

emptyLine :: Parser ()
emptyLine = skipWhile isLinespace *> lineEnding_

optIndent :: Parser String
optIndent = atMostN 3 (char ' ')

printable :: Parser Char
printable = satisfy (not . isSpace)

punctuation :: Parser Char
punctuation = satisfy isPunctuation

control :: Parser Char
control = satisfy isControl

next :: Parser a -> Parser a
next p = skipWhile isLinespace *> p

escape :: Char -> Parser Char
escape c = char '\\' *> char c

escaped :: Parser Char
escaped = char '\\' *> satisfy (\c -> isPunctuation c || isSymbol c)

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (\c -> c `elem` cs)

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (\c -> c `notElem` cs)

-- Combinators

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

optionMaybe :: Alternative f => f a -> f (Maybe a)
optionMaybe p = option Nothing $ Just <$> p

manyN :: Int -> Parser a -> Parser [a]
manyN n p
  | n <= 0 = return []
  | otherwise = liftM2 (++) (count n p) (many p)

manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p end = liftA2 (:) p (manyTill p end)

atMostN :: Int -> Parser a -> Parser [a]
atMostN n p
  | n <= 0 = count 0 p
  | otherwise = count n p <|> atMostN (n - 1) p

atMostN1 :: Int -> Parser a -> Parser [a]
atMostN1 n p
  | n <= 1 = count 1 p
  | otherwise = count n p <|> atMostN1 (n - 1) p

-- Useful functions

isWhitespace :: Char -> Bool
isWhitespace c = isSpace c

isLinespace :: Char -> Bool
isLinespace c = isSpace c && (not . isLineEnding) c

isLineEnding :: Char -> Bool
isLineEnding c = c == '\n' || c == '\r'

isPrintable :: Char -> Bool
isPrintable = not . isSpace

-- Doubt

linkLabel :: Parser Text
linkLabel = T.pack <$> between (char '[') (char ']') (many1 $ escape '[' <|> escape ']' <|> noneOf "[]")

linkDestination :: Parser Text
linkDestination =
  T.pack <$> between (char '<') (char '>') (many betweenChar)
  <|>
  T.concat <$> many (destText <|> parens)
  where destText = T.pack <$> many1 destChar
        parens   = do
          open <- string "("
          content <- destText <|> return ""
          close <- string ")"
          return $ T.concat [open, content, close]
        betweenChar = escaped <|> satisfy (\c -> not $ isWhitespace c || c  == '<' || c == '>')
        destChar    = escaped <|> satisfy (\c -> not $ isWhitespace c || isControl c || c == '(' || c == ')')

linkTitle :: Parser Text
linkTitle = titleOf '"' '"' <|> titleOf '\'' '\'' <|> titleOf '(' ')'
  where titleOf :: Char -> Char -> Parser Text
        titleOf open close =
          T.pack <$>
          between (char open) (char close) (many $ escape open <|> escape close <|> noneOf [open, close])
