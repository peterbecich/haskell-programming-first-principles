{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config { counts :: IORef (M.Map Text Integer), prefix :: Text }

type Scotty = ScottyT Text (StateT (M.Map Text Integer) (ReaderT Config IO))
type Handler = ActionT Text (ReaderT Config IO)

-- https://hackage.haskell.org/package/containers-0.5.10.1/docs/Data-Map-Strict.html

increment :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
increment k m = let
  m' = M.alter (\maybeCount -> case maybeCount of
                            (Just count) -> Just (count + 1)
                            Nothing -> Just 1
                        ) k m
  in (m', fromMaybe 0 (M.lookup k m'))

-- ScottyT Text (ReaderT Config IO) ()
-- app :: Scotty ()
-- app =
--   get "/:key" $ do
--   unprefixed <- param "key"
--   let key' = mappend prefix unprefixed
--   counts 
--   newInteger <- increment key' counts
--   html $ mconcat [ "<h1>Success! Count was: "
--                  , TL.pack $ show newInteger
--                  , "</h1>"
--                  ]

-- -- https://hackage.haskell.org/package/scotty-0.11.0/docs/Web-Scotty-Trans.html

-- main :: IO ()
-- main = do
--   [prefixArg] <- getArgs
--   ioRef <- newIORef M.empty
--   let config = Config { counts = ioRef, prefix = "foo" }
--       runR = undefined
--   scottyT 3000 runR app

