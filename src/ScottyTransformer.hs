{-# LANGUAGE OverloadedStrings #-}

module ScottyTransformer where

import Web.Scotty

import Control.Monad.Trans.Class

import Data.Monoid (mconcat)

main = scotty 3000 $ do
  -- get :: RoutePattern -> ActionM () -> ScottyM ()
  get "/:word" $ do
    beam <- param "word"
    -- :t lift
    -- lift :: (Monad m, MonadTrans t) => m a -> t m a
    -- :t lift :: IO a -> ActionM a
    -- lift :: IO a -> ActionM a :: IO a -> ActionM a
    lift (putStrLn "hello")
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
