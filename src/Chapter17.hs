module Chapter17 where

import Control.Applicative
import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- ($) :: (a -> b) -> a -> b
-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

foo = [(*2), (*3)] <*> [4, 5]


-- :info (,)
-- data (,) a b = (,) a b 	-- Defined in ‘ghc-prim-0.5.0.0:GHC.Tuple’
-- ...
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’
-- ...
-- instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
-- instance Foldable ((,) a) -- Defined in ‘Data.Foldable’
-- instance Traversable ((,) a) -- Defined in ‘Data.Traversable’
-- instance (Monoid a, Monoid b) => Monoid (a, b)
--   -- Defined in ‘GHC.Base’

bar = ("woo", (+1)) <*> (" hoo!", 0 :: Int)
-- ("woo hoo!",1)

x = ((Sum 2), (+1)) <*> ((Sum 0), 0)
y = ((Product 3), (+9)) <*> ((Product 2), 8)
z = ((All True), (+1)) <*> ((All False), 0)

-- :t liftA
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- :t liftA2
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c


-- applicative laws
-- identity   pure id <*> v = v
-- composition   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- homomorphism   pure f <*> pure x = pure (f x)
-- 



-- see ZipListMonoid.hs

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap func (Cons x xs) = Cons (func x) (fmap func xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) (Cons x xs) = Cons (f x) (fs <*> xs)

trigger = undefined :: (String, String, String)  

testList = quickBatch $ applicative [trigger]

-- testList' = quickBatch $ applicative (Cons trigger Nil)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap func lx = concat' $ fmap func lx

toMyList = foldr Cons Nil
xs = toMyList [1, 2, 3]

c = Cons

foo' = flatMap (\x -> x `c` (9 `c` Nil)) xs

count :: Int -> List Int
count 0 = Cons 0 (count 1)
count i = Cons i (count (i + 1))

zipperHelp :: (a -> b -> c) -> a -> (List c, List b) -> (List c, List b)
zipperHelp f x (lz , (Cons y0 ys0)) =
  (Cons (f x y0) lz , ys0)
zipperHelp _ _ (lz , Nil) = (lz , Nil)

zipper :: (a -> b -> c) -> List a -> List b -> List c
zipper f lx ly = fst $ fold (zipperHelp f) (Nil, ly) lx

zipTuple :: List a -> List b -> List (a, b)
zipTuple lx ly = zipper (,) lx ly

zipWithIndex :: List a -> List (a, Int)
zipWithIndex lx = zipTuple lx (count 0)

-- do it with fold, rather than recursively, like in FP in Scala
-- this won't `take` infinite lists
-- need a fold that can be terminated by the helper function
takeHelper :: Int -> (a, Int) -> List a -> List a
takeHelper n (x, i) lx
  | n > 0 = Cons x lx
  | otherwise = lx

-- take' :: Int -> List a -> List a
-- take' n lx = fold (takeHelper n) xs (zipWithIndex lx)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n lx@(Cons x xs)
  | n >= 0 = (Cons x (take' (n - 1) xs))
  | otherwise = lx

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
              in take' 3000 l
          ys' = let (ZipList' l) = ys
              in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ pure x
  (<*>) (ZipList' f) (ZipList' x) = ZipList' $ f <*> x


z' = ZipList' $ toMyList [(+9), (*2), (+8)]
z'' = ZipList' $ toMyList [1..3]

z''' = z' <*> z''

-- trigger = undefined :: (String, String, String)

-- testZipList = quickBatch $ applicative (ZipList' (Cons trigger Nil))

