module Chapter16 where

a = fmap (+1) $ read "[1]" :: [Int]

b :: Maybe [String]
b = fmap (fmap (++ "lol")) (Just ["Hi, ", "Hello "])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        cast = fmap show ioi :: IO String
        prepended = fmap ("123"++) cast
        changed = fmap read prepended
    in fmap (*3) changed


-- :info Functor 
-- class Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a

-- 1

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

data Quant a b = Finance | Desk a | Floor b

-- instance Functor (Flip Functor b) where
--   fmap _ Finance = Finance
--   fmap f (Desk x) = Desk $ f x
--   fmap _ flo@(Floor y) = flo

