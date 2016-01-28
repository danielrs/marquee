module Text.ParserCombinators.Parsec.Marquee where

import Control.Monad (liftM2)
import Text.ParserCombinators.Parsec

none :: Parser ()
none = return ()

manyTill1 :: Parser a -> Parser b -> Parser [a]
manyTill1 p end = do
  x <- p
  xs <- manyTill p end
  return (x:xs)

manyN :: Int -> Parser a -> Parser [a]
manyN n p
  | n <= 0 = return []
  | otherwise = liftM2 (++) (count n p) (many p)

atMostN :: Int -> Parser a -> Parser [a]
atMostN n p
  | n <= 0 = count 0 p
  | otherwise = try (count n p) <|> atMostN (n - 1) p

atMostN1 :: Int -> Parser a -> Parser [a]
atMostN1 n p
  | n <= 1 = count 1 p
  | otherwise = try (count n p) <|> atMostN1 (n - 1) p
