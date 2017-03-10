module Chapter25 where

import Course.Compose

newtype Identity a = Identity { runIdentity :: a }

x = Compose [Just (1 :: Int), Nothing]

-- :t x
-- x :: Compose [] Maybe Int

-- needs Traversable
-- instance (Monad f, Monad g) => Monad (Compose f g) where
--   return = pure
--   (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
--   (>>=) = ???

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fgx) = undefined

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse func (Compose fgx) = undefined


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
  return = pure
  (>>=) (Identity x) f = f x

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  (<*>) (IdentityT fab) (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (>>=) (IdentityT mx) f = IdentityT $ mx >>= runIdentityT . f


-- :k Compose 
-- Compose :: (* -> *) -> (* -> *) -> * -> *  
