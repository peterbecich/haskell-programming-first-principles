{-# LANGUAGE InstanceSigs #-}

module Chapter22 where

import Control.Applicative
import Data.Char
import Data.Maybe

hurr :: Num a => a -> a
hurr = (*2)
durr :: Num a => a -> a
durr = (+10)

-- m :: Integer -> Integer
m :: Num a => a -> a
m = hurr . hurr

-- m' :: Integer -> Integer
m' :: Num a => a -> a
m' = fmap hurr durr

m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

thirteen = ((+) <$> (*2)) 5 3
nineteen = ((+) <$> (*2) <*> (+10)) 3


-- :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- :t (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
-- (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)
--       :: (a -> a -> b) -> (a -> a) -> a -> b

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)

-- monadic context
hurrDurr :: Integer -> Integer
hurrDurr = do
  a <- hurr
  b <- durr
  return (a + b)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

--  composed "foo"
-- "OOF"
--  fmapped "foo"
-- "OOF"

tupled :: [Char] -> ([Char], [Char])
tupled = cap >>= (\x -> rev >>= (\y -> return (x, y)))

  -- do
  -- x <- cap
  -- y <- rev
  -- return (x, y)

--  tupled "foo"
-- ("FOO","oof")

--  :info Functor 
-- class Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   	-- Defined in ‘GHC.Base’
-- instance Functor ZipList -- Defined in ‘Control.Applicative’
-- instance Monad m => Functor (WrappedMonad m)
--   -- Defined in ‘Control.Applicative’
-- instance Control.Arrow.Arrow a => Functor (WrappedArrow a b)
--   -- Defined in ‘Control.Applicative’
-- instance Functor (Const m) -- Defined in ‘Control.Applicative’
-- instance Functor (Either a) -- Defined in ‘Data.Either’
-- instance Functor [] -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance Functor IO -- Defined in ‘GHC.Base’
-- instance Functor ((->) r) -- Defined in ‘GHC.Base’  <<<<<<<<<<<<<<<<<<<<<<<<<<<<
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’


-- :info (->)
-- data (->) a b 	-- Defined in ‘ghc-prim-0.4.0.0:GHC.Prim’
-- instance Monad ((->) r) -- Defined in ‘GHC.Base’
-- instance Functor ((->) r) -- Defined in ‘GHC.Base’
-- instance Applicative ((->) a) -- Defined in ‘GHC.Base’
-- instance Monoid b => Monoid (a -> b) -- Defined in ‘GHC.Base’

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  --fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

ask :: Reader a a
ask = Reader id

-- pure :: a -> f a
-- pure :: a -> (r -> a)
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pat :: Person
pat =
  Person (HumanName "Pat") (DogName "Junior") (Address "Long Beach")

peter :: Person
peter =
  Person (HumanName "Peter") (DogName "Junior") (Address "Long Beach")

-- without reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- :t Dog 
-- Dog :: DogName -> Address -> Dog
-- :t dogName 
-- dogName :: Person -> DogName
-- :t address 
-- address :: Person -> Address

-- :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- with reader
getDogR :: Person -> Dog
getDogR = liftA2 Dog dogName address

junior = getDogR pat


liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' func fx fy = let
  fbc = (pure func) <*> fx -- :: f ( b -> c )
  in fbc <*> fy

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\_ -> a)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rab) (Reader rx) = Reader $ (\r -> let
                                                ab = rab r
                                                x = rx r
                                                in ab x
                                            )

getDogR' :: Reader Person Dog
getDogR' = liftA2 Dog (Reader dogName) (Reader address)

junior' = runReader getDogR' pat

instance Monad (Reader r) where
  return = pure
  (>>=) readerA func = Reader $ (\r0 -> runReader (func (runReader readerA r0)) r0 )

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addr <- address
  return $ Dog name addr

getDogRM' :: Reader Person Dog
getDogRM' = do
  name <- Reader dogName
  addr <- Reader address
  return $ Dog name addr

junior'' = runReader getDogRM' pat

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]


