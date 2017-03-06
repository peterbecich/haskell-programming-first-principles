module Chapter18 where

import Control.Monad

-- :t liftM 
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- :t liftM2
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

x = liftM2 (+) [3,4] [5,6]
--y = liftM2 (+) (Sum 3, Sum 4) (Sum 5, Sum 6)
y = liftM2 (,) [3,4] [5,6]
z = liftM2 (,,) [3,4] [5,6]

printOne = putStrLn "1"
printTwo = putStrLn "2"
printOneAndTwo :: (IO (), IO ())
printOneAndTwo = (printOne, printTwo)

-- only prints 2
printOneAndTwo' = sequence printOneAndTwo

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = let
  -- mb :: m b
  mb = g a
  in mb >>= f


-- :t flip (.)
-- flip (.) :: (a -> b) -> (b -> c) -> a -> c
-- :t (>=>)
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

-- :t read 
-- read :: Read a => String -> a  

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"

data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg
  
instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

data PhhhbbtttEither b a = Left' a | Right' b

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' x) = Right' x
  fmap f (Left' x) = Left' $ f x

instance Applicative (PhhhbbtttEither a) where
  pure x = Left' x
  (<*>) _ (Right' x) = (Right' x)
  (<*>) (Left' ab) (Left' x) = Left' $ ab x
  --(<*>) (Right' ab) (Left' x) = Left' $ ab x
  -- incomplete coverage?

newtype Identity' a = Identity' a deriving (Eq, Ord, Show)

instance Functor Identity' where
  fmap f (Identity' x) = Identity' $ f x

instance Applicative Identity' where
  pure x = Identity' x
  (<*>) (Identity' ab) (Identity' x) = Identity' $ ab x

instance Monad Identity' where
  return x = Identity' x
  (>>=) (Identity' x) f = f x

-- ex 4
data List' a = Nil' | Cons' a (List' a)



