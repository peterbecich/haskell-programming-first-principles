module Chapter16 where


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

main :: IO ()
main = do
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
