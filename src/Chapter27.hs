module Chapter27 where

one = fst (1, undefined)

true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

possiblyKaboom = \f -> f fst snd (0, undefined)

possiblyKaboom' = possiblyKaboom true


hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "put 'hi'"

-- demonstrate eagerness with `undefined`s, not with infinite loops

-- seq :: a -> b -> b
hypo' :: IO ()
hypo' = do
  let x :: Integer
      x = undefined
  s <- getLine
  case x `seq` s of
    "hi" -> print x
    _ -> putStrLn "put 'hi'"

-- seq :: a -> b -> b
-- seq (bottom x) = bottom
-- seq (anythingNotBottom x) = x

wc x z = let y = undefined `seq` 'y' in x
letterA = foldr wc 'z' ['a'..'e']
letterZ = foldr (flip wc) 'z' ['a'..'e']

bot = undefined

wc' x z = let y = bot `seq` 'y' in y `seq` x
letterA' = foldr wc' 'z' ['a'..'e']
letterZ' = foldr (flip wc') 'z' ['a'..'e']
