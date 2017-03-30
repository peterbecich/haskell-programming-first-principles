{-# LANGUAGE OverloadedStrings #-}

module ScottyTransformer3 where

import Web.Scotty

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as TL

param' :: Parsable a => TL.Text -> ExceptT String ActionM a
param' k = ExceptT $
  rescue (Right <$> param k)
  (const
   (return
    (Left $ "The key: " ++ (show k) ++ " was missing")
   )
  )


type Reco = (Integer, Integer, Integer, Integer)

main = scotty 3000 $ do
  get "/" $ do
    reco <- runExceptT $ do
      a <- param' "1"
      liftIO $ print a
      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    case reco of
      (Left e) -> text (TL.pack e)
      (Right r) -> html $ mconcat ["<h1>Success! Reco: ", (TL.pack . show) r, "</h1"]

-- http://localhost:3000/?1=100&2=200&3=300&4=400
-- Success! Reco: (100,200,300,400)

-- http://localhost:3000/?1=100&2=200&3=300
-- The key: "4" was missing

-- http://localhost:3000/?1=100&2=200
-- The key: "3" was missing
-- doesn't accumulate errors... because of do-notation
