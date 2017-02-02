module Chapter4 where
import GHC.Int

data Bool' = False' | True' deriving (Show)

not' :: Bool' -> Bool'
not' False' = True'
not' True' = False'


-- specify the type of a
-- minBound :: Bounded a => a
intMinBound = minBound :: Int

bigIntMaxBound = maxBound :: Int64

data Things = First | Second | Third deriving (Show, Ord, Eq)

--  First < Second
-- True
--  Second > First
-- True
--  Second < First
-- False
--  


--  :info Things
-- data Things = First | Second | Third
--   	-- Defined at /Users/peterbecich/haskell/haskell-programming-first-principles/.stack-work/intero/intero42659HGQ.hs:17:1
-- instance [safe] Eq Things
--   -- Defined at /Users/peterbecich/haskell/haskell-programming-first-principles/.stack-work/intero/intero42659HGQ.hs:17:59
-- instance [safe] Ord Things
--   -- Defined at /Users/peterbecich/haskell/haskell-programming-first-principles/.stack-work/intero/intero42659HGQ.hs:17:54
-- instance [safe] Show Things
--   -- Defined at /Users/peterbecich/haskell/haskell-programming-first-principles/.stack-work/intero/intero42659HGQ.hs:17:48

