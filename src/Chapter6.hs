
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



