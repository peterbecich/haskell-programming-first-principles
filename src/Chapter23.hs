module Chapter23 where

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Fizz"
  | n `mod` 3 == 0 = "Buzz"
  | otherwise = show n

fizz :: IO ()
fizz = mapM_ (putStrLn . fizzBuzz) [1..30]
