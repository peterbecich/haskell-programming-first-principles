module Chapter26 where

import Data.Functor
-- https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Data-Functor-Compose.html
-- import Data.Functor.Compose
import Control.Applicative

import Data.Either

-- newtype Compose' f g a = 

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- from NICTA course
-- instance (Functor f, Functor g) => Functor (Compose f g) where
--   fmap func (Compose fga) = Compose $ (\ga -> (func <$> ga)) <$> fga


    --Compose $ liftA2 (\gab ga -> liftA2 (\ab a -> ab a) gab ga) fgab fga

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  -- fmap f (Right x) = Right $ f x
  fmap f et = EitherT $ (fmap . fmap) f (runEitherT et)


-- :t (liftA2 . liftA2)
-- (liftA2 . liftA2)
--   :: (Applicative f, Applicative f1) =>
--      (a -> b -> c) -> f (f1 a) -> f (f1 b) -> f (f1 c)

-- :t (<*>) . (<*>)
-- (<*>) . (<*>)
--   :: (a -> a1 -> b) -> ((a -> a1) -> a) -> (a -> a1) -> b
--  :t (<*>) (<*>)
-- (<*>) (<*>)
--   :: Applicative f => (f (a -> b) -> f a) -> f (a -> b) -> f b
--  :t liftA2 . (<*>)
-- liftA2 . (<*>)
--   :: Applicative f => (b -> a -> c) -> f (b -> a) -> f b -> f c
--  :t (<*>) . liftA2
-- (<*>) . liftA2
--   :: Applicative f => (a -> b -> c) -> (f a -> f b) -> f a -> f c
  
instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure (Right x)
  (<*>) etfab etfa = let
    fEitherAB = runEitherT etfab -- :: m ((Either e a) -> b)
    fEitherA = runEitherT etfa -- :: m (Either e a)
    in EitherT $ (liftA2 . liftA2) (\f x -> f x) fEitherAB fEitherA

instance Monad m => Monad (EitherT e m) where
  return = pure
  -- (>>=) ::
  --   (EitherT m (Either e a)) ->
  --   (a -> EitherT m (Either e b)) ->
  --   (EitherT m (Either e b))
  (>>=) eitherTMEitherA func = let
    mEitherA = runEitherT eitherTMEitherA -- :: m (Either e a)
    -- efunc :: Either e a -> EitherT m (Either e b)
    efunc (Right a) = func a
    efunc (Left e) = EitherT ( pure (Left e) )
    in EitherT $ mEitherA >>= (runEitherT . efunc)
