module Text.ParserCombinators.Parsec.Marquee where

import Control.Monad (liftM2)
import Text.Parsec (Parsec(..))
import Text.ParserCombinators.Parsec

none :: Parsec String u ()
none = return ()

manyTill1 :: (Show b) => Parsec String u a -> Parsec String u b -> Parsec String u [a]
manyTill1 p end = p >>= \x -> manyTill p end >>= \xs -> return (x:xs)

manyN :: Int -> Parsec String u a -> Parsec String u [a]
manyN n p
  | n <= 0 = return []
  | otherwise = liftM2 (++) (count n p) (many p)

atMostN :: Int -> Parsec String u a -> Parsec String u [a]
atMostN n p
  | n <= 0 = return []
  | otherwise = try (count n p) <|> atMostN (n - 1) p

atMostN1 :: Int -> Parsec String u a -> Parsec String u [a]
atMostN1 n p
  | n <= 1 = count 1 p
  | otherwise = try (count n p) <|> atMostN1 (n - 1) p
