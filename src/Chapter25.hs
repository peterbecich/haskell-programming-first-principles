-- {-# LANGUAGE NoImplicitPrelude #-}
module Chapter25 where

-- import Course.Core
-- import Course.Functor
-- import Course.Applicative
-- import Course.Monad
import Course.Compose

import Data.Foldable
import Data.Traversable
import Data.Maybe

-- import Prelude (undefined, ($), (.), id)

newtype Identity a = Identity { runIdentity :: a }

-- x = Compose [Just (1 :: Int), Nothing]

-- :t x
-- x :: Compose [] Maybe Int

-- needs Traversable
-- instance (Monad f, Monad g) => Monad (Compose f g) where
--   return = pure
--   (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
--   (>>=) = ???

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  -- foldMap :: (Foldable (Compose f g), Monoid m) => (a -> m) -> Compose f g a -> m
  -- have at disposal:
  -- foldMap :: (Foldable f, Monoid m) => (b -> m) -> f b -> m
  -- foldMap :: (Foldable g, Monoid m) => (c -> m) -> f c -> m
  foldMap f (Compose fgx) = foldMap (\gx -> (foldMap f gx)) fgx

instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  -- (<$>) :: (a -> b) -> Compose f g a -> Compose f g b
  fmap func ( Compose fga ) = Compose $ (fmap . fmap) func fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  -- traverse :: (Applicative h, Traversable (Compose f g)) =>
  --             (a -> h b) -> Compose f g a -> h (Compose f g b)
  -- have at disposal: 
  -- traverse :: (Applicative h, Traversable f) =>
  --             (a -> h b) -> f a -> h (f b)  
  -- traverse :: (Applicative h, Traversable g) =>
  --             (a -> h b) -> g a -> h (g b)
  traverse func (Compose fgx) = undefined --traverse (\ga 


class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second f = bimap id f

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)

data Const a b = Const a

instance Bifunctor Const where
  bimap f g (Const x) = Const $ f x

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
  bimap f _ (Left' x) = Left' $ f x
  bimap _ g (Right' y) = Right' $ g y

-- 25.8

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity $ f x

instance Monad Identity where
  (>>=) (Identity x) f = f x

newtype IdentityT f a = IdentityT { runIdentityT :: f a } -- deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (<*>) (IdentityT fab) (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  -- (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (>>=) (IdentityT mx) f = IdentityT $ mx >>= runIdentityT . f
  -- (>>=) (IdentityT mx) f = let
  --   f' b = mx >>= f  -- :: 
  --   in undefined


-- :k Compose 
-- Compose :: (* -> *) -> (* -> *) -> * -> *


