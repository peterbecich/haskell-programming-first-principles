module Chapter26 where

import Data.Functor
-- https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Data-Functor-Compose.html
-- import Data.Functor.Compose
import Control.Applicative

-- newtype Compose' f g a = 

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- from NICTA course
-- instance (Functor f, Functor g) => Functor (Compose f g) where
--   fmap func (Compose fga) = Compose $ (\ga -> (func <$> ga)) <$> fga


    --Compose $ liftA2 (\gab ga -> liftA2 (\ab a -> ab a) gab ga) fgab fga
