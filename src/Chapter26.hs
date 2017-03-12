module Chapter26 where

import Data.Functor
-- https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Data-Functor-Compose.html
-- import Data.Functor.Compose
import Control.Applicative

import Data.Either
import Data.Bifunctor

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

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT et = let
  mEitherEA = runEitherT et
  in EitherT $ (fmap) swapEither mEitherEA

swapEither :: Either e a -> Either a e
swapEither (Right x) = Left x
swapEither (Left x) = Right x

foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither f1 _ (Left x) = f1 x
foldEither _ f2 (Right y) = f2 y

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f1 f2 eitherTAMB = let
  mEitherAB = runEitherT eitherTAMB
  in (foldEither f1 f2) =<< mEitherAB


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f rt = ReaderT (\r0 -> let
                          mx0 = runReaderT rt r0
                          in fmap f mx0
                      )

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT (\_ -> pure x)
  (<*>) rtab rta = ReaderT (\r0 -> let
                               mab = runReaderT rtab r0 -- :: m (a -> b)
                               mx = runReaderT rta r0 -- :: m a
                               in mab <*> mx
                           )

instance Monad m => Monad (ReaderT r m) where
  return = pure
  --                        (a -> ReaderT { runReaderT :: r -> m b })
  -- (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) rta aRTB = let
    rma = runReaderT rta -- :: r -> m a
    armb = runReaderT . aRTB -- :: a -> r -> m b
    ramb = flip armb -- :: r -> a -> m b
    -- r -> m b
    rmb r = let
      ma = rma r -- :: m a
      amb = ramb r -- :: a -> m b
      in ma >>= amb
    in ReaderT rmb
                               

