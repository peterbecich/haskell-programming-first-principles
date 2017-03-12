module Chapter23 where

import Course.State
-- import System.Random

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


-------------------

-- newtype State s a = State { runState :: s -> (a, s) }

-- random :: (Random a) => StdGen -> (a, StdGen)

data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

-- rollDieThreeTimes :: (Die, Die, Die)
-- rollDieThreeTimes = do
--   let s = mkStdGen 0  -- threading states manually
--       (d1, s1) = randomR (1,6) s
--       (d2, s2) = randomR (1,6) s1
--       (d3, s3) = randomR (1,6) s2
--   (intToDie d1, intToDie d2, intToDie d3)

-- rollDie :: State StdGen Die
-- rollDie = state $

