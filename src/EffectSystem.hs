module EffectSystem where

import Data.IORef
import Control.Monad
import Control.Monad.ST
import Data.STRef
-- http://blog.jakubarnold.cz/2014/07/20/mutable-state-in-haskell.html

-- data IORef a

-- newIORef    :: a -> IO (IORef a)
-- readIORef   :: IORef a -> IO a
-- writeIORef  :: IORef a -> a -> IO ()
-- modifyIORef :: IORef a -> (a -> a) -> IO ()

main :: IO ()
main = do
  ref <- newIORef (0 :: Int)
  _ <- modifyIORef ref (+1)
  readIORef ref >>= print

bubbleSort :: [Int] -> IO [Int]
bubbleSort input = do
  let ln = length input
  -- xs :: IORef ([Int])
  xs <- mapM newIORef input
  _ <- forM_ [0..(ln-1)] $ \_ -> do
    forM_ [0..(ln-2)] $ \j -> do
      let ix = xs !! j
      let iy = xs !! (j+1)
      x <- readIORef ix
      y <- readIORef iy
      when (x > y) $ do
        _ <- writeIORef ix y
        _ <- writeIORef iy x
        return ()
  mapM readIORef xs


nums = [3,5,9,1,2,4,2,2,6,9,99]
numsSorted = bubbleSort nums

-- data STRef s a

-- newSTRef    :: a -> ST s (STRef s a)
-- readSTRef   :: STRef s a -> ST s a
-- writeSTRef  :: STRef s a -> a -> ST s ()
-- modifySTRef :: STRef s a -> (a -> a) -> ST s ()

-- runST :: (forall s. ST s a) -> a

magic :: Int -> Int
magic x = runST $ do
  ref <- newSTRef x
  _ <- modifySTRef ref (+1)
  readSTRef ref

three = magic 2

bubbleSort' :: [Int] -> [Int]
bubbleSort' input = runST $ do
  let ln = length input
  -- xs :: [STRef s Int]
  xs <- mapM newSTRef input
  _ <- forM_ [0..(ln-1)] $ \_ -> do
    forM_ [0..(ln-2)] $ \j -> do
      let sx = xs !! j
          sy = xs !! (j+1)
      x <- readSTRef sx
      y <- readSTRef sy
      when (x > y) $ do
        _ <- writeSTRef sx y
        _ <- writeSTRef sy x
        return ()
  mapM readSTRef xs

numsSorted' = bubbleSort' nums


-- http://stackoverflow.com/questions/20439316/when-to-use-stref-or-ioref
-- http://stackoverflow.com/questions/12468622/how-does-the-st-monad-work/12468757#12468757
exampleSTRef :: ST s Int
exampleSTRef = do
  counter <- newSTRef 0
  _ <- modifySTRef counter (+1)
  readSTRef counter

one = runST exampleSTRef

-- https://en.wikipedia.org/wiki/Quicksort#Lomuto_partition_scheme

quickSort :: [Int] -> [Int]
quickSort xs = runST $ let
  lo = 0
  hi = (length xs) - 1
  sxs = return xs
  in quickSort' sxs lo hi

quickSort' :: (ST s [Int]) -> Int -> Int -> (ST s [Int])
quickSort' sxs low high = if (low < high)
  then do
  p <- partition sxs low high
  _ <- quickSort' sxs low (p-1)
  quickSort' sxs (p+1) high
  else sxs



-- when :: Applicative f => Bool -> f () -> f ()
partition :: (ST s [Int]) -> Int -> Int -> ST s Int
partition sxs low high = do
  xs <- sxs
  let pivot = xs !! high
  refi <- newSTRef (low - 1)
  forM_ [low..(high-1)] $ \j -> do
    i <- readSTRef refi
    let jth = xs !! j
    when (jth <= pivot)
      (modifySTRef refi (+1))
    undefined
  i <- readSTRef refi
  return (i+1)


quicksorted = quickSort nums


