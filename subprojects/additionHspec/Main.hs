module Main where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

someInts = sample (arbitrary :: Gen Int)
someDoubles = sample (arbitrary :: Gen Double)
genIntStrings = fmap show (arbitrary :: Gen Int)
genDoubleStrings = fmap show (arbitrary :: Gen Double)

mixedNumbers :: Gen String
mixedNumbers = oneof [genIntStrings, genDoubleStrings]
someMixedNumbers = sample mixedNumbers

oneToThree :: Gen Int
oneToThree = elements [1, 2, 3]
sampleOneToThree = sample oneToThree

data Die = Heads | Tails deriving Show
weightedDie :: Gen Die
weightedDie = elements [Heads, Tails, Tails, Tails]

throwWeightedDie = sample weightedDie

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is greater than 3" $ do
      (2 + 2) > 3 `shouldBe` True
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
    it "x + 1 is always greater than x" $ do
      property $ \x -> (x + 1 > (x :: Int)) && (x < 4)
      
  describe "Division" $ do
    it "15 divided by 5 is 3" $ do
      dividedBy 15 5 == (3, 0) `shouldBe` True
      dividedBy 15 5 /= (3, 0) `shouldBe` False

