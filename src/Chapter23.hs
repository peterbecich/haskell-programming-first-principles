module Chapter23 where

import Course.State

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Fizz"
  | n `mod` 3 == 0 = "Buzz"
  | otherwise = show n

fizz :: IO ()
fizz = mapM_ (putStrLn . fizzBuzz) [1..30]

wilma = runState (put "Wilma") "Daphne"

modify :: (s -> s) -> State s ()
modify f = State $ (\s1 -> ((), s1)) . f

one' = runState (modify (+1)) 0
