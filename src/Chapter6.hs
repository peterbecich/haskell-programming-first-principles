
module Chapter6 where

class Monoid' a where
  zero :: a
  combine :: a -> a -> a

instance Monoid' Int where
  zero = 0
  combine x y = x + y

instance Monoid' [a] where
  zero = []
  combine listA listB = listA ++ listB

foldRight :: [a] -> (a -> b -> b) -> b -> b
foldRight [] _ z = z
foldRight (h:t) f z = f h $ foldRight t f z

reduceRight :: Monoid' a => [a] -> a
reduceRight list = foldRight list combine zero

fiftyFive :: Int
fiftyFive = reduceRight [1..10]

combinedWords = reduceRight ["foo","bar","baz"]

-- https://twitter.com/peterbecich/status/692247564452773890
-- https://twitter.com/kmett/status/692397185648959491


-- https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor.html
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b


-- https://www.haskell.org/tutorial/classes.html
-- Haskell also supports a notion of class extension. For example, we may wish to define a class Ord which inherits all of the -- operations in Eq, but in addition has a set of comparison operations and minimum and maximum functions:

-- class  (Eq a) => Ord a  where
--   (<), (<=), (>=), (>)  :: a -> a -> Bool
--   max, min              :: a -> a -> a

class Functor' m => Monad' m where
  pure' :: a -> m a
  bind' :: m a -> (a -> m b) -> m b
  -- fmap' :: (a -> b) -> m a -> m b
  -- fmap' f ma = bind' ma $ pure' . f

-- instance Monad' m where
--   fmap' :: (a -> b) -> m a -> m b
--   fmap' f ma = bind' ma $ pure' . f
  
  
