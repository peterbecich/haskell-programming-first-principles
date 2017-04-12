module Chapter9 where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs

myOr :: [Bool] -> Bool
myOr xs = foldr (||) True xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> b || (f x)) False xs

myElem :: Eq a => a -> [a] -> Bool
myElem x' xs = myAny (\x -> x == x') xs

