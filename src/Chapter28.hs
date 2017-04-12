module Chapter28 where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

-- iterate :: (a -> a) -> a -> [a]
s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

benchmarkSet :: IO ()
benchmarkSet = defaultMain
  [ bench "member check map " $ whnf membersMap 9999
  , bench "member check set " $ whnf membersSet 9999
  ]

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = return v
        go n v = (MV.write v n 0) >> go (n-1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n+1)
  go n mvec
  where go 0 v = V.freeze v
        go n v = (MV.write v n 0) >> go (n-1) v

benchmarkMutableVector :: IO ()
benchmarkMutableVector = defaultMain
  [ bench "mutable IO vector" $ whnfIO (mutableUpdateIO 9998)
  , bench "mutable ST vector" $ whnf mutableUpdateST 9998
  ]
