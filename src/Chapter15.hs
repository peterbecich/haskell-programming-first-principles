module Chapter15 where

import Control.Monad.Trans.Writer.Lazy
import Data.Monoid

-- :info Monoid 
-- class Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a

one = Sum 1

three = one `mappend` one `mappend` one

four = mconcat [one, one, one, one]

fourtyEight = mconcat ([2, 4, 6] :: [Product Int])
fourtyEight' = mconcat ([Product 2, Product 4, Product 6] :: [Product Int])

nums :: [Sum Int]
nums = Sum <$> [1..10] 

nums' :: [(Sum Int, Product Int)]
nums' = (\i -> (Sum i, Product i)) <$> [1..10] 

fiftyFive :: Sum Int
fiftyFive = foldr mappend mempty nums

five = one <> one <> one <> one <> one

-- https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Base.html#line-278
sumAndProduct :: (Sum Int, Product Int)
sumAndProduct = foldr mappend mempty nums'

data Booly a = False' | True' deriving (Eq, Show)

instance Monoid (Booly a) where
  mempty = True'
  -- AND monoid
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'


data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only x) (Only y) = Only $ mappend x y
  mappend Nada o2@(Only _) = o2
  mappend o1@(Only _) Nada = o1
  mappend Nada Nada = Nada


-- instance Monoid [a] where
--     mappend = (++)
--     mempty = []
a = []

-- http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
-- It works just as well, but there is a big advantage to using the Writer version. It has type signature f :: Integer -> Writer (Sum Integer) Integer.
-- We can immediately read from this that our function has a side effect that involves accumulating a number in a purely additive way. It's never going to,
-- for example, multiply the accumulated value. The type information tells us a lot about what is going on inside the function without us having to read a
-- single line of the implementation. The version written with State is free to do whatever it likes with the accumulated value and so it's harder to discern
-- its purpose.

fact2 :: Integer -> Writer (Sum Integer) Integer
fact2 0 = return 1
fact2 n = do
  let n' = n-1
  tell $ Sum 1
  m <- fact2 n'
  let r = n*m
  tell $ Sum 1
  return r

ex2 = runWriter (fact2 10)

