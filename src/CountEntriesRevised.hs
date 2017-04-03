{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CountEntriesRevised where

-- http://book.realworldhaskell.org/read/monad-transformers.html#id657557

import System.Directory
import System.FilePath
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig { cfgMaxDepth :: Int } deriving (Show)

data AppState = AppState { stDeepestReached :: Int } deriving (Show)

-- the `a` is unnecessary
-- it is present implicitly, if not explicitly
type App a = ReaderT AppConfig (StateT AppState IO) a

pth = "/home/peterbecich/haskell/haskell-programming-first-principles/src"
pth2 = "/home/peterbecich/haskell/haskell-programming-first-principles"
pth3 = "/Users/peterbecich/haskell/haskell-programming-first-principles/"
pth4 = "/Users/peterbecich/haskell/haskell-programming-first-principles/src"

runApp :: App a -> Int -> IO (a, AppState)
runApp app maxDepth =
  let config = AppConfig maxDepth
      startState = AppState 0
  in runStateT (runReaderT app config) startState

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
    let newPath = path </> name -- concatenate strings
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
      let newDepth = curDepth + 1
      st <- get
      when (stDeepestReached st < newDepth) $
        put st { stDeepestReached = newDepth }
      constrainedCount newDepth newPath
    else return []
  return $ (path, length contents) : concat rest

constrainedCount' = constrainedCount 0 pth3


-- run' ::

run' :: IO ([(FilePath, Int)], AppState)
run' = runApp constrainedCount' 2

-- newtype MyApp a = MyA {
--   runA :: App a
--   } deriving (Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

newtype MyApp a = MyA {
  runA :: App a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

myApp = MyA $ constrainedCount 0 pth3

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp myApp maxDepth =
  let config = AppConfig maxDepth
      startState = AppState 0
  in runStateT (runReaderT (runA myApp) config) startState

-- myApp = MyA 

