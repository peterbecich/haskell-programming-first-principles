{-# LANGUAGE OverloadedStrings #-}

module ScottyTransformer2 where

import Web.Scotty

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text, pack)
import Data.Monoid (mconcat)

param' :: Parsable a => Text -> MaybeT ActionM a
param' k = MaybeT $ rescue (Just <$> param k) (const (return Nothing))

type Reco = (Integer, Integer, Integer, Integer)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    reco <- runMaybeT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    liftIO $ print reco
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>", "   Reco: ", (pack (show reco))]

-- http://localhost:3000/beam?1=100&2=200&3=300&4=400
-- Scotty, beam me up!
-- Reco: Just (100,200,300,400)

-- http://localhost:3000/beam?1=100&2=200&3=300
-- Scotty, beam me up!
-- Reco: Nothing
