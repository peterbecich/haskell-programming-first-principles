{-# LANGUAGE OverloadedStrings #-}

module ScottyTransformer where

import Web.Scotty

import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Monoid (mconcat)

-- :info ActionM
-- type ActionM =
--   Web.Scotty.Internal.Types.ActionT Data.Text.Internal.Lazy.Text IO
--   :: * -> *
--   	-- Defined in ‘Web.Scotty’

param' :: Parsable a => Text -> ActionM (Maybe a)
param' k = rescue (Just <$> param k) (const (return Nothing))

main = scotty 3000 $ do
  -- get :: RoutePattern -> ActionM () -> ScottyM ()
  get "/:word" $ do
    beam' <- param' "word"
    let beam = fromMaybe "" beam'
    i <- param' "num"
    liftIO $ print $ "beam': " ++ show beam'    
    liftIO $ print $ "beam: " ++ show beam
    liftIO $ print $ "num: " ++ show (i :: Maybe Integer)
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]









