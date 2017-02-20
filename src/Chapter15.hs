module Chapter15 where

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


