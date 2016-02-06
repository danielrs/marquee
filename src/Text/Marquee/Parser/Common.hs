module Text.Marquee.Parser.Common where

import Control.Applicative
import Control.Monad

import Data.Char (isControl, isPunctuation, isSpace)
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

optIndent :: Parser String
optIndent = atMostN 3 (char ' ')

printable :: Parser Char
printable = satisfy (not . isSpace)

punctuation :: Parser Char
punctuation = satisfy isPunctuation

control :: Parser Char
control = satisfy isControl

next :: Parser a -> Parser a
next p = skipWhile isWhitespace *> p

escaped :: Char -> Parser Char
escaped c = char '\\' *> char c

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (\c -> c `elem` cs)

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (\c -> c `notElem` cs)

-- Combinators

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

-- Useful functions

isWhitespace :: Char -> Bool
isWhitespace c = isSpace c && c /= '\n'

isLineEnding :: Char -> Bool
isLineEnding c = c == '\n' || c == '\r'

isPrintable :: Char -> Bool
isPrintable = not . isSpace

-- Doubt

linkLabel :: Parser Text
linkLabel = T.pack <$> between (char '[') (char ']') (many1 $ escaped '[' <|> escaped ']' <|> noneOf "[]")

-- linkDestination :: Parser Text
-- linkDestination = Atto.takeWhile1 (\c -> not $ isSpace c || isControl c || c == '(' || c == ')')

linkDestination :: Parser Text
linkDestination =
  T.pack <$>
  between (char '<') (char '>') (many $ escaped '<' <|> escaped '>' <|> noneOf "<>")
  <|>
  takeWhile1 (\c -> not $ isSpace c || isControl c)

linkTitle :: Parser Text
linkTitle = titleOf '"' '"' <|> titleOf '\'' '\'' <|> titleOf '(' ')'
  where titleOf :: Char -> Char -> Parser Text
        titleOf open close =
          T.pack <$>
          between (char open) (char close) (many $ escaped open <|> escaped close <|> noneOf [open, close])
