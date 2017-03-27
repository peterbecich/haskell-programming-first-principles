module Chapter17 where

import Control.Applicative
import Data.Monoid

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




-- instance EqProp Bull where
--   (=-=) Fools Fools = True
--   (=-=) Twoo Twoo = True
--   (=-=) _ _ = False

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
