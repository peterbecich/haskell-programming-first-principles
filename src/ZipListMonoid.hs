module ZipListMonoid where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  -- `pure` of the Applicative ZipList instance
  mempty = pure mempty -- mempty of a
  -- `liftA2` of the Applicative ZipList instance
  -- liftA2 mappend :: (Monoid c, Applicative f) => f c -> f c -> f c
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

main :: IO ()
main = quickBatch $ monoid (ZipList [1 :: Sum Int])



