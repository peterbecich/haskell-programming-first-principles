module Chapter20 where

import Data.Foldable
import Data.Monoid

x = [1..10]

sum' :: (Foldable t, Num a) => t a -> a
sum' tx = getSum $ foldMap Sum tx

sumX = sum' x

product' :: (Foldable t, Num a) => t a -> a
product' tx = getProduct $ foldMap Product tx

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x tx = foldr (\y b -> (x == y) || b) False tx

minimumHelper :: (Ord a) => a -> Maybe a -> Maybe a
minimumHelper x Nothing = Just x
minimumHelper x jsty@(Just y)
  | x < y = Just x
  | otherwise = jsty

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' tx = foldr minimumHelper Nothing tx


maximumHelper :: (Ord a) => a -> Maybe a -> Maybe a
maximumHelper x Nothing = Just x
maximumHelper x (Just y) = Just $ max x y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' tx = foldr maximumHelper Nothing tx

--maximum' tx = foldr (\x mybe -> min ( fmap . fmap ) (Just x) mybe) Nothing tx

length' :: (Foldable t) => t a -> Int
length' tx = getSum $ foldMap (\_ -> (Sum 1)) tx

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f tx = foldr (mappend . f) mempty tx

sum'' :: (Foldable t, Num a) => t a -> a
sum'' tx = getSum $ foldMap' Sum tx

sumX' = sum'' x

fold' :: (Foldable t, Monoid m) => t m -> m
fold' tx = foldMap' id tx

x' = fmap Sum x
sumX'' = getSum $ fold' x'

-- data Constant a b = Constant a
-- instance Foldable (Constant a) -- flip type ... fix b (Constant _ b)
