{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SimpleTransformer where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
-- import Control.Monad.Trans.State.Lazy
-- import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.Thread.Delay
import Data.Time.Clock
import Data.UUID
import Data.UUID.V4

-- http://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe


greet :: IO ()
greet = do
  _ <- putStrLn "What is your name? "
  name <- getLine
  putStrLn $ "Hello " ++ name


-- liftIO :: MonadIO m => IO a -> m a

mgreet :: MaybeT IO ()
mgreet = do
  _ <- liftIO $ putStr "What is your name? "
  name <- liftIO getLine
  liftIO $ putStrLn $ "Hello " ++ name

mgreet' :: IO (Maybe ())
mgreet' = runMaybeT mgreet

endlessGreeting = forever mgreet

endlessGreeting' :: IO (Maybe ())
endlessGreeting' = runMaybeT endlessGreeting


askFor :: String -> IO String
askFor prompt = do
  putStr $ "What is your " ++ prompt ++ "?"
  getLine

survey :: IO (String, String)
survey = do
  name <- askFor "name"
  color <- askFor "favorite color"
  return (name, color)

askfor1 :: String -> IO (Maybe String)
askfor1 prompt = do
  _ <- putStr $ "What is your " ++ prompt ++ " (type END to quit)?"
  response <- getLine
  if response == "END"
    then return Nothing
    else return (Just response)

-- staircasing issue
survey1 :: IO (Maybe (String, String))
survey1 = do
  maybeName <- askfor1 "name"
  case maybeName of
    Nothing -> return Nothing
    (Just name) -> do
      maybeColor <- askfor1 "favorite color"
      case maybeColor of
        (Just color) -> return (Just (name, color))
        Nothing -> return Nothing

-- better
askFor2 :: String -> MaybeT IO String
askFor2 prompt = do
  liftIO $ putStr $ "what is your " ++ prompt ++ " (type END to quit)?"
  response <- liftIO getLine
  if response == "END"
    then MaybeT (return Nothing)
    else MaybeT (return (Just response))

survey2 :: MaybeT IO (String, String)
survey2 = do
  name <- askFor2 "name"
  favoriteColor <- askFor2 "favorite color"
  return (name, favoriteColor)

survey2' :: IO (Maybe (String, String))
survey2' = runMaybeT survey2

askFor3 :: String -> MaybeT IO String
askFor3 prompt = do
  response <- liftIO $ do
    _ <- putStrLn $ "What is your " ++ prompt ++ " (type END to quit)?"
    getLine
  if response == "END"
    then mzero
    else return response

survey3 :: MaybeT IO (String, String)
survey3 = do
  name <- askFor3 "name"
  color <- askFor3 "color"
  return (name, color)

loop1 :: IO ()
loop1 = do
  _ <- putStr "Password: "
  pass <- getLine
  if pass == "SECRET"
    then return ()
    else loop1

loop2 :: IO (Maybe ())
loop2 = runMaybeT $
  forever $
  do
    _ <- liftIO $ putStr "Password: "
    pass <- liftIO $ getLine
    if pass == "SECRET"
      then mzero
      else return ()

authenticate :: MaybeT IO UUID
authenticate =  do
    _ <- liftIO $ putStr "Password: "
    pass <- liftIO $ getLine
    if pass == "SECRET"
      then liftIO nextRandom
      else authenticate

authenticate' = runMaybeT authenticate

-- authenticate2 :: MaybeT IO UUID
-- authenticate2 = forever $ do
--     _ <- liftIO $ putStr "Password: "
--     pass <- liftIO $ getLine
--     if pass == "SECRET"
--       then liftIO nextRandom
--       else return ()

-- authenticate2' = runMaybeT authenticate2


------------------------------------------------

-- IO is overkill here
simpleCounter :: StateT Integer IO ()
simpleCounter = modify (\i -> i + 2)

count20 = replicateM 10 simpleCounter

-- execStateT count20 0
-- 20

data Config = Config { add :: Integer } deriving (Show)

type Adder = ReaderT Config (StateT Integer IO)

config = Config 2

startState :: Integer
startState = 0

adder :: Adder ()
adder = do
  config <- ask
  let
    toAdd = add config
  -- lift $ do
  --   s <- get
  --   let s' = s + toAdd
  --   put s'
  s <- lift get
  let
    s' = s + toAdd
  lift $ put s'

runAdderStateT :: Config -> StateT Integer IO ()
runAdderStateT config = runReaderT adder config

runAdder :: IO ((), Integer)
runAdder = runStateT (runAdderStateT config) startState

execAdder :: IO Integer
execAdder = execStateT (runAdderStateT config) startState

-- replicateM 10 execAdder 
-- [2,2,2,2,2,2,2,2,2,2]
  
addTenTimes :: Adder ()
addTenTimes = replicateM_ 10 adder

execAdder' :: IO Integer
execAdder' = execStateT (runReaderT adder config) startState


-- addSecond :: Adder ()
-- addSecond = do
--   startTime <- ask
--   let diff = diffUTCTime 
--   _ <- modify 

-- https://www.youtube.com/watch?v=pzouxmWiemg

-- https://youtu.be/pzouxmWiemg?t=1363

-- newtype App e c a = App (EitherT e (ReaderT c IO) a)

data SqlError = SqlError String

data AppError = DbSqlException SqlError | MissingFoo

data AppEnv = AppEnv { appPort :: Int }

newtype App a = App (ExceptT AppError (ReaderT AppEnv IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader AppEnv
    , MonadError AppError
    , MonadIO
    )

-- runApp :: AppEnv -> App a -> IO (Either AppError a)
-- runApp e = runExceptT . flip runReaderT e . 


