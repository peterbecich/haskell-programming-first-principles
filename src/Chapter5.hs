module Chapter5 where

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

five = uncurry' (+) (2,3)
five' = curry' (uncurry' (+)) 2 3

plusUncurried :: Num a => (a, a) -> a
plusUncurried = uncurry' (+)

plusRecurried :: Num a => a -> a -> a
plusRecurried = curry' plusUncurried

--     • Couldn't match type ‘(Integer -> Integer -> Integer, Integer)’
--                      with ‘a0 -> b0 -> c’
-- five'' = curry' uncurry' (+) 2 3

triple x = tripleItYo x
  where tripleItYo :: Int -> Int
        tripleItYo y = y*3
nine = triple 3
