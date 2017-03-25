module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

import Control.Applicative

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
oneEOF = one >> eof

two = char '2'
two' = two >> stop

twoEOF :: Parser Char
twoEOF = two <* eof

-- type Parser a = String -> Maybe (a, String)
--             ^ token type
--                    ^ input String
--                                      ^ remainder of string, assuming parsing hasn't failed
--                                  ^ token


oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

-- successful after one
oneTwoEOF :: Parser Char
oneTwoEOF = one <* twoEOF
--oneTwoEOF = one >>= (\c -> fmap (\_ -> c) twoEOF)


testParse :: Parser Char -> IO ()
testParse prsr = print $ parseString prsr mempty "123"

parse :: Show a => Parser a -> String -> IO ()
parse prsr input = print $ parseString prsr mempty input

pNL s = putStrLn ('\n' : s)

main = do
  putStrLn "--------------"
  pNL "stop:"
  testParse stop
  putStrLn "--------------"  
  pNL "one:"
  testParse one
  putStrLn "--------------"  
  pNL "one':"
  testParse two
  putStrLn "--------------"  
  pNL "oneTwo:"
  testParse oneTwo
  putStrLn "--------------"  
  pNL "oneTwo':"
  testParse oneTwo'
  putStrLn "===================="
  putStrLn "parsing 1"
  pNL "oneTwoEOF:"
  parse oneTwo "1"
  putStrLn "--------------"
  putStrLn "parsing 142"
  pNL "oneTwoEOF:"
  parse oneTwo "142"
  putStrLn "--------------"
  putStrLn "parsing 12"
  pNL "oneTwo:"
  parse oneTwo "12"
  putStrLn "--------------"
  putStrLn "parsing 12"
  pNL "oneTwoEOF:"
  parse oneTwoEOF "12"

-- note these parsers can only produce a single token
-- the type of these parsers is Parser Char, after all
-- a list of tokens might be Parser [Char]

-- stringParser :: Parser Char -> Parser Char -> Parser String
-- stringParser px py = do
--   x <- px
--   y <- py
--   return (x:[y])

stringP :: Parser Char -> Parser String
stringP pc = fmap (\c -> [c]) pc

oneS = stringP (char '1')
twoS = stringP (char '2')
threeS = stringP (char '3')

oneTwoThreeS :: Parser String
oneTwoThreeS =
  liftA3 (\x y z -> x ++ y ++ z) oneS twoS threeS

oneTwoS :: Parser String
oneTwoS = liftA2 (++) oneS twoS

countOfThree :: Parser String
countOfThree =
  choice [oneTwoThreeS, oneTwoS, oneS] <* eof

countThree = do
  putStrLn "--------------"  
  parse countOfThree "1234"
  putStrLn "--------------"  
  parse countOfThree "123"
  putStrLn "--------------"  
  parse countOfThree "12"
  putStrLn "--------------"  
  parse countOfThree "23"
  putStrLn "================"  
  parse oneTwoS "12"
  putStrLn "--------------"  
  parse oneTwoS "123"
  putStrLn "--------------"    
  parse (oneTwoS <* eof) "12"
  putStrLn "--------------"    
  parse (oneTwoS <* eof) "123"


secondChoiceP = choice [oneTwoThreeS, oneTwoS]
secondChoice = do
  parse secondChoiceP "123"
  putStrLn "--------------"  
  parse secondChoiceP "12"
  putStrLn "--------------"  
  parse secondChoiceP "1"
