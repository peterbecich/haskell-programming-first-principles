{-# LANGUAGE OverloadedStrings #-}
-- https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md
module GentleIntroMonadTransfomers where

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Data.Functor
import Control.Applicative

-- data LoginError = InvalidEmail deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _ -> Left InvalidEmail

peter = "peter@foo.com"
peter' = "peter.com"

peterDomain = getDomain peter
peterDomain' = getDomain peter'
         
printResult :: Either LoginError Text -> IO ()
printResult = T.putStrLn . (either (const "ERROR: invalid email") (append "Domain: "))

getToken :: IO (Either LoginError Text)
getToken = do
  _ <- T.putStrLn "Enter email address: "
  email <- T.getLine
  return (getDomain email)

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password"), ("foo.com", "1234")]

data LoginError = InvalidEmail | NoSuchUser | WrongPassword deriving Show

userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken

  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userpw -> do
          T.putStrLn "Enter password: "
          password <- T.getLine

          if userpw == password
            then return token
            else return $ Left WrongPassword
        Nothing -> return (Left NoSuchUser)
    left -> return left

data EitherIO e a = EitherIO {
  runEitherIO :: IO (Either e a)
}

-- instance Show (EitherIO e a) where
--   show eio = "EitherIO"
  
-- :t EitherIO
-- EitherIO :: IO (Either e a) -> EitherIO e a
-- :t runEitherIO
-- runEitherIO :: EitherIO e a -> IO (Either e a)

instance Functor (EitherIO e) where
  fmap f eio = wrapped
    where -- use this as much as `let`
      unwrapped = runEitherIO eio
      fmapped = fmap (fmap f) unwrapped
      wrapped = EitherIO fmapped
      
  --fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure = EitherIO . return. Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)
  --     referring to Either's ^^^

instance Monad (EitherIO e) where
  return = pure
  -- either :: (a -> c) -> (b -> c) -> Either a b -> c
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)


getToken'' :: IO (Either LoginError Text)
getToken'' = do
  _ <- T.putStrLn "Enter e-mail address: "
  input <- T.getLine
  return $ getDomain input
  
wrapIO :: IO a -> EitherIO e a
wrapIO io = EitherIO $ fmap Right io

-- getDomain :: Text -> Either LoginError Text
getToken' :: EitherIO LoginError Text
getToken' = do
  _ <- wrapIO $ T.putStrLn "Enter e-mail address: "
  input <- wrapIO T.getLine
  EitherIO $ return $ getDomain input
  --         ^^^ referring to IO

liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO (return x)

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)

getToken''' :: EitherIO LoginError Text
getToken''' = do
  _ <- liftIO (T.putStrLn "Enter email address: ")
  input <- liftIO T.getLine
  liftEither (getDomain input)

-- maybe :: b -> (a -> b) -> Maybe a -> b
-- Map.lookup :: Ord k => k -> Map k a -> Maybe a
userLogin' :: EitherIO LoginError Text
userLogin' = do
  token <- getToken'''
  userpw <- maybe (liftEither (Left NoSuchUser)) return (Map.lookup token users)
  password <- liftIO (T.putStrLn "Enter your password: " >> T.getLine)
  if userpw == password then (return token) else liftEither (Left WrongPassword)


printResult' :: Either LoginError Text -> IO ()
printResult' res =
  T.putStrLn $ case res of
                 Right token -> append "Logged in with token: " token
                 Left InvalidEmail -> "Invalid e-mail address"
                 Left NoSuchUser -> "No user with that e-mail address"
                 Left WrongPassword -> "Wrong password"

prompt :: IO ()
prompt = do
  -- response :: Either LoginError Text
  response <- runEitherIO userLogin'
  printResult' response

throwE :: e -> EitherIO e a
throwE x = liftEither (Left x)

userLogin'' :: EitherIO LoginError Text
userLogin'' = do
  token <- getToken'''
  userpw <- maybe (throwE NoSuchUser) return (Map.lookup token users)
  password <- liftIO $ T.putStrLn "Enter your password: " >> T.getLine

  if userpw == password
    then return token
    else throwE WrongPassword
    
prompt' = do
  response <- runEitherIO userLogin''
  printResult' response

-- same as ExceptT

catchE :: EitherIO e a -> (e -> EitherIO e a) -> EitherIO e a
catchE throwing handler =
  EitherIO $ do
  result <- runEitherIO throwing
  case result of
    Left failure -> runEitherIO (handler failure)  -- handler may also fail
    success -> return success

wrongPasswordHandler :: LoginError -> EitherIO LoginError Text
wrongPasswordHandler WrongPassword = do
  _ <- liftIO (T.putStrLn "Wrong password, one more chance")
  userLogin''
wrongPasswordHandler err = throwE err -- any other error is *not* caught

printError' :: LoginError -> EitherIO LoginError a
printError' err = do
  _ <- liftIO . T.putStrLn $ case err of
    WrongPassword -> "Wrong password.  No more chances"
    NoSuchUser -> "No user with that email exists"
    InvalidEmail -> "Invalid email address entered"
  throwE err

loginDialogue :: EitherIO LoginError ()
loginDialogue = do
  let retry :: EitherIO LoginError Text
      retry = catchE userLogin'' wrongPasswordHandler
  token <- catchE retry printError'
  liftIO $ T.putStrLn (append "Logged in with token: " token)

prompt'' = runEitherIO loginDialogue


