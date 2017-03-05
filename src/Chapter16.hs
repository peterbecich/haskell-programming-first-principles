module Chapter16 where
import Test.QuickCheck
import Test.QuickCheck.Function


data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor (FixMePls) where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- identity
-- fmap id == id
hi = fmap id "Hello Peter"
hi' = id "Hello Peter"
-- composition

f1 = (+1)
f2 = (*2)
-- fmap (f . g) == fmap f . fmap g
x = fmap (f1 . f2) [1..5]
y = fmap f1 . fmap f2 $ [1..5]

lms = [Just "Ave", Nothing, Just "woohoo"]
replaceWithP = const 'p'

lms' = fmap replaceWithP lms
lms'' = (fmap . fmap) replaceWithP lms
lms''' = (fmap . fmap . fmap) replaceWithP lms

-- nicer than <$> ... <$> ... <$>

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f0) => f0 (f1 a) -> f0 (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f0) => f0 (f1 (f2 a)) -> f0 (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main' :: IO ()
main' = do
  putStrLn "[Maybe [Char]] -> Char"
  print (replaceWithP' lms)

  putStrLn "Functor f => f a -> f Char"
  print (liftedReplace lms)

  putStrLn "[Maybe [Char]] -> [Char]"
  print (liftedReplace' lms)

  putStrLn "(Functor f1, Functor f0) => f0 (f1 a) -> f0 (f1 Char)"
  print (twiceLifted lms)

  putStrLn "[Maybe [Char]] -> [Maybe Char]"
  print (twiceLifted' lms)

  putStrLn "(Functor f2, Functor f1, Functor f0) => f0 (f1 (f2 a)) -> f0 (f1 (f2 Char))"
  print (thriceLifted lms)

  putStrLn "[Maybe [Char]] -> [Maybe [Char]]"
  print (thriceLifted' lms)


-- exercises

a = fmap (+1) $ read "[1]" :: [Int]

b :: Maybe [String]
b = fmap (fmap (++ "lol")) (Just ["Hi, ", "Hello "])

c = (*2) . (\x -> x - 2)
c' = c 1

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        cast = fmap show ioi :: IO String
        prepended = fmap ("123"++) cast
        changed = fmap read prepended
    in fmap (*3) changed


data Two a b = Two a b deriving (Eq, Show)
data Or a b = First a | Second b deriving (Eq, Show)

-- "fix" the first type argument.  It will be a functor over b, not a
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Functor (Or a) where
  fmap _ first@(First x) = First x -- first didn't work
  fmap f (Second y) = Second (f y)
  
  
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorIdentityListInt = (\x -> functorIdentity (x :: [Int]))
functorComposeListInt = (\x -> functorCompose (+1) (*2) (x :: [Int]))
--                                              ^^  ^^ generate these
-- quickCheck functorIdentityListInt 
-- +++ OK, passed 100 tests.
-- quickCheck functorComposeListInt
-- +++ OK, passed 100 tests.


-- :t Fun 
-- Fun :: (a :-> b, b) -> (a -> b) -> Fun a b

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == ((fmap g . fmap f) x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- quickCheck (functorCompose' :: IntFC)

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- TODO
-- functorIdentityIdentityInt = (\x -> functorIdentity (x :: (Identity Int)))
-- functorComposeIdentityInt = (\x -> functorCompose (+1) (*2) (x :: (Identity Int)))

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x1 x2) = Pair (f x1) (f x2)

getInt :: IO Int
getInt = fmap read getLine

multiplied = fmap show (fmap (*10) getInt) >>= putStrLn

