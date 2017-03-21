{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as ByteChar
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TextLazy
import qualified Database.Redis as Redis
import Network.URI (URI, parseURI)
import qualified System.Random as SystemRandom
import Web.Scotty
import Data.Monoid (mconcat)
-- import Data.List.NonEmpty
import Control.Monad
import Control.Applicative

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex = length xs - 1 :: Int
  randomDigit <- SystemRandom.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit) -- !! is unsafe

sevenRandomChars :: IO ByteChar.ByteString
sevenRandomChars = fmap ByteChar.pack $ replicateM 7 (randomElement alphaNum)

-- type NonEmptyList a = (a, [a])
sevenList :: IO [ByteChar.ByteString]
sevenList = liftM2 (\x xs -> x:xs) sevenRandomChars sevenList

-- sevenRandomChars' :: IO (NonEmptyList String)
-- sevenRandomChars' = 



-- want short URLs that don't exist in Redis yet
foldEither :: Either Redis.Reply Bool -> Bool
foldEither (Left _) = True
foldEither (Right True) = False
foldEither (Right False) = True

redisPredicate redisConnection short =
  fmap foldEither $ Redis.runRedis redisConnection (Redis.exists short)

-- filterM' :: Applicative m => (a -> m Bool) -> m a -> 
-- rconn :: Redis.Connection
-- rconn = error "todo"
-- filterM (\short -> (redisPredicate rconn short))
--   :: [ByteChar.ByteString] -> IO [ByteChar.ByteString]


generateShort :: Redis.Connection -> IO ByteChar.ByteString
generateShort redisConnection =
  fmap head $ sevenList >>= filterM (\short -> (redisPredicate redisConnection short))

-- Redis.runRedis :: Redis.Connection -> Redis.Redis a -> IO a
-- Redis.set
--   :: Redis.RedisCtx m f =>
--      ByteChar.ByteString -> ByteChar.ByteString -> m (f Redis.Status)

-- https://hackage.haskell.org/package/hedis-0.6.5/docs/Database-Redis.html#v:set

saveURI :: Redis.Connection
        -> ByteChar.ByteString
        -> ByteChar.ByteString
        -> IO (Either Redis.Reply Redis.Status)
saveURI conn shortURI longURI = Redis.runRedis conn $ Redis.set shortURI longURI

getURI :: Redis.Connection
       -> ByteChar.ByteString
       -> IO (Either Redis.Reply (Maybe ByteChar.ByteString))
getURI conn shortURI = Redis.runRedis conn $ Redis.get shortURI

linkShorty :: String -> String
linkShorty shorty = concat [ "<a href=\"", shorty, "\">", shorty, "</a>" ]

shortyCreated :: Show a => a -> String -> TextLazy.Text
shortyCreated response shorty = let
  prefix = "http://localhost:3000/lengthen/"
  in TextLazy.concat [ TextLazy.pack (show response), " shorty is: ", TextLazy.pack (linkShorty (prefix++shorty)) ]

shortyNotURL :: TextLazy.Text -> TextLazy.Text
shortyNotURL uri = TextLazy.concat [ uri, " wasn't a URL"]

shortyFound :: TextLazy.Text -> TextLazy.Text
shortyFound tbs = TextLazy.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

-- getAction = do
--   uri <- param "uri"
  
-- get :: RoutePattern -> ActionM () -> ScottyM ()

app :: Redis.Connection -> ScottyM ()
app redisConnection = do
  get "/" $ do
    html $ mconcat ["<h1>Scotty, beam me up!</h1>"]
  get "/shorten/:word" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TextLazy.unpack uri)
    case parsedUri of
      Just _ -> do
        shorty <- liftIO (generateShort redisConnection)
        let uri' = encodeUtf8 (TextLazy.toStrict uri)
        response <- liftIO (saveURI redisConnection shorty uri')
        html (shortyCreated response (ByteChar.unpack shorty))
      Nothing -> text (shortyNotURL uri)
  get "/lengthen/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI redisConnection short)
    case uri of
      (Left reply) -> text (TextLazy.pack (show reply))
      (Right maybeLongURI) -> case maybeLongURI of
        Nothing -> text "URI not found"
        (Just longURI) -> html (shortyFound tbs)
          where tbs :: TextLazy.Text
                tbs = TextLazy.fromStrict (decodeUtf8 longURI)

-- redisConnectionInfo

main = do
  redisConnection <- Redis.connect Redis.defaultConnectInfo
  scotty 3000 (app redisConnection)


  
-- main :: ScottyM ()
-- https://hackage.haskell.org/package/scotty
-- main = scotty 3000 $ do
--   get "/:word" $ do
--     beam <- param "word"
--     html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
