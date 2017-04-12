module Vigenere where

import Caesar (letters, letterShift, letterIndex, letter, space, caesarCipher)

import Data.Char
import Data.Functor
import Data.List
import Data.Foldable
import Control.Applicative

repeatKeyword :: [Char] -> [Char]
repeatKeyword xs = xs ++ (repeatKeyword xs)

-- https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher

vigenereCipher :: [Char] -> [Char] -> [Char]
vigenereCipher keyword message = let

  keywordInfinite :: [Char]
  keywordInfinite = repeatKeyword keyword

  keywordIndicesInfinite :: [Maybe Int]
  keywordIndicesInfinite = fmap letterIndex keywordInfinite

  messageIndices :: [Maybe Int]
  messageIndices = fmap letterIndex message

  zipped :: [(Maybe Int, Maybe Int)]
  zipped = zip messageIndices keywordIndicesInfinite

  sums :: [Maybe Int]
  sums = fmap (uncurry (\mi1 mi2 -> liftA2 (+) mi1 mi2)) zipped

  folded :: [Int]
  folded = fmap (\i -> foldr const space i) sums

  zipped' = zip folded message

  shifted = fmap (uncurry letterShift) zipped'

  in shifted


