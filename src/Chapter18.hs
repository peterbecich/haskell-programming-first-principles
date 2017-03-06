module Chapter18 where

import Control.Monad

-- :t liftM 
-- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- :t liftM2
-- liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

x = liftM2 (+) [3,4] [5,6]
--y = liftM2 (+) (Sum 3, Sum 4) (Sum 5, Sum 6)
y = liftM2 (,) [3,4] [5,6]
z = liftM2 (,,) [3,4] [5,6]

printOne = putStrLn "1"
printTwo = putStrLn "2"
printOneAndTwo :: (IO (), IO ())
printOneAndTwo = (printOne, printTwo)

-- only prints 2
printOneAndTwo' = sequence printOneAndTwo



