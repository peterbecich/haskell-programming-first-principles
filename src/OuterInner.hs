module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

out = readerUnwrap ()

readerRewrapped :: ReaderT () IO (Either String (Maybe Int))
readerRewrapped = ReaderT readerUnwrap

eitherRewrapped :: ExceptT String (ReaderT () IO) (Maybe Int)
eitherRewrapped = ExceptT readerRewrapped

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT eitherRewrapped
