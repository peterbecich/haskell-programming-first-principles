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



