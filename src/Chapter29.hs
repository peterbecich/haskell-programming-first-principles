module Chapter29 where

import Control.Concurrent

import Data.Functor
import Control.Applicative
import Control.Monad (replicateM)
import System.Random
import Debug.Trace

import Data.Time.Calendar
import Data.Time.Clock

myData :: IO (MVar Int)
myData = newEmptyMVar

mvarExample :: IO ()
mvarExample = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero

blah :: IO String
blah = return "blah"

blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

traceExample :: IO ()
traceExample = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w
  

gimmeShelter :: Bool -> IO [Int]
gimmeShelter True = replicateM 10 (randomRIO (0, 10))
gimmeShelter False = pure [0]

helloPerson = (++) <$> getLine <*> getLine

huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) = toGregorian (utctDay t)
  case even dayOfMonth of
    True -> return $ Left randomIO
    False -> return $ Right (putStrLn "no soup for you")

-- huehue' = fmap show huehue >>= print

mvarExample' :: IO ()
mvarExample' = do
  mv <- newEmptyMVar
  putMVar mv (0 :: Int)
  zero <- takeMVar mv
  print zero



