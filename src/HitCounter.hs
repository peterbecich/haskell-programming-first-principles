module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config { counts :: IORef (M.Map Text Integer), prefix :: Text }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

-- https://hackage.haskell.org/package/containers-0.5.10.1/docs/Data-Map-Strict.html

increment :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
increment k m = let
  m' = M.alter (\maybeCount -> case maybeCount of
                            (Just count) -> Just (count + 1)
                            Nothing -> Just 1
                        ) k m
  in (m', fromMaybe 0 (M.lookup k m'))
--increment k m = M.update (\i -> Just (i + 1)) k m
