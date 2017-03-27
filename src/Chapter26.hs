module Chapter26 where

import Data.Functor
-- https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Data-Functor-Compose.html
-- import Data.Functor.Compose
import Control.Applicative
-- import Control.Monad.Trans.Class
import Data.Either
import qualified Data.Bifunctor as Bfunctor

import Course.StateT

import Chapter25

-- import Control.Monad.Trans.Either

-- newtype Compose' f g a = 

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f maybeTx = MaybeT $ (fmap . fmap) f (runMaybeT maybeTx)

-- liftA . liftA
--   :: (Applicative f1, Applicative f) =>
--      (a -> b) -> f (f1 a) -> f (f1 b)

-- liftA2 . liftA2
--   :: (Applicative f1, Applicative f) =>
--      (a -> b -> c) -> f (f1 a) -> f (f1 b) -> f (f1 c)

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ pure $ Just x
  (<*>) maybeTab maybeTx = let
    mMaybeAB = runMaybeT maybeTab
    mMaybeX = runMaybeT maybeTx
    in MaybeT $ (liftA2 . liftA2) (\f x -> f x) mMaybeAB mMaybeX

instance (Monad m) => Monad (MaybeT m) where
  return x = MaybeT $ return $ Just x
  (>>=) maybeTA aMaybeTB = MaybeT $ do
    maybeX <- runMaybeT maybeTA
    let
      maybeAMaybeTB (Just x) = aMaybeTB x
      maybeAMaybeTB Nothing = MaybeT $ pure Nothing
    runMaybeT $ maybeAMaybeTB maybeX


  -- (>>=) maybeTA aMaybeTB = let
  --   mMaybeA = runMaybeT maybeTA
  --   maybeAMMaybeTB (Just x) = pure (aMaybeTB x)
  --   maybeAMMaybeTB Nothing = pure $ MaybeT $ pure Nothing
  --   mMaybeTB = mMaybeA >>= maybeAMMaybeTB
  --   in mMaybeTB >>= runMaybeT
  
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



instance Functor f => Functor (StateT s f) where
  -- fmap :: (a -> b) -> StateT s f a -> StateT s f b
  fmap func stateTSFA = StateT (\s0 -> let
                                    fas = runStateT stateTSFA s0 -- :: f (a, s)
                                    in fmap (Bfunctor.bimap func id) fas
                                )


-- Bfunctor.first . liftA
--   :: (Bfunctor.Bifunctor p, Applicative f) =>
--      (a -> b) -> p (f a) c -> p (f b) c

-- liftA . Bfunctor.first
--   :: (Bfunctor.Bifunctor p, Applicative f) =>
--      (a -> b) -> f (p a c) -> f (p b c)

-- instance (Applicative m) => Applicative (StateT s m) where
--   pure x = StateT $ \s0 -> pure (x, s0)
  -- (<*>) stateTSFAB stateTSFA = StateT $ \s0 ->
  --   let
  --     mabs = runStateT stateTSFAB -- :: m (a -> b, s)
  --     smas = runStateT stateTSFA -- :: s -> m (a, s)
  --     func (ab, s1) = (liftA . Bfunctor.first) ab (smas s1)
  --   in mabs >>= func
    
    -- StateT $ (\s0 -> do
    --              (ab, s1) <- runStateT stateTSFAB s0
    --              (a, s2) <- runStateT stateTSFA s1
    --              pure (ab a, s2)
    --          )

-- need Monad m, rather than Applicative m    
instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s0 -> pure (x, s0)
  (<*>) stateTSFAB stateTSFA = StateT $ \s0 ->
    let
      mabs = runStateT stateTSFAB s0 -- :: m (a -> b, s)
      smas = runStateT stateTSFA -- :: s -> m (a, s)
      -- (a -> b, s) -> m (b, s)
      func (ab, s1) = (liftA . Bfunctor.first) ab (smas s1)
    in mabs >>= func -- :: m (b, s)

instance (Monad m) => Monad (StateT s m) where
  return x = StateT $ \s0 -> pure (x, s0)
  (>>=) stateTSMA aStateTSMB = StateT $ \s0 -> do
    (a, s1) <- runStateT stateTSMA s0
    -- let stateTSMB = aStateTSMB a -- :: StateT s m b
    -- runStateT stateTSMB 
    runStateT (aStateTSMB a) s1
    
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

-- given any Monad m, lift it into Monad Transformer t       
class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans (ReaderT r ) where
  lift = ReaderT . const

instance MonadTrans (EitherT e ) where
  lift mx = EitherT $ fmap Right mx

instance MonadTrans (StateT s) where
  lift mx = StateT $ \s0 -> fmap (\x -> (x, s0)) mx

-- liftIO

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

-- IdentityT defined in Chapter25
instance (MonadIO m) => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

----

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO iox = MaybeT $ fmap Just (liftIO iox)

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO iox = ReaderT $ \_ -> (liftIO iox)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO iox = StateT $ \s0 -> fmap (\x -> (x, s0)) (liftIO iox)
