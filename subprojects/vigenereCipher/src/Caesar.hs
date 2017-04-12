module Caesar where

import Data.Char
import Data.Functor
import Data.List

-- :t chr
-- chr :: Int -> Char
-- :t ord 
-- ord :: Char -> Int

lowerLetters = ['a'..'z']
upperLetters = ['A'..'Z']
letters = lowerLetters ++ upperLetters ++ [' ']

space :: Int
space = length letters - 1

-- letterLoop :: [Char]
-- letterLoop = letters ++ letterLoop

letter n = letters !! (mod n (length letters))

letterIndex :: Char -> Maybe Int
letterIndex c = elemIndex c letters

letterShift :: Int -> Char -> Char
letterShift shift c =
  let maybeI = elemIndex c letters
  in case maybeI of
    (Just i) -> letter (i + shift)
    Nothing -> ' '


caesarCipher :: Int -> [Char] -> [Char]
caesarCipher shift xs = fmap (letterShift shift) xs


caesarListens = "Caesar listens"
secret = caesarCipher 5 caesarListens
caesarListens' = caesarCipher (-5) secret

