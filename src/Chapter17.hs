module Chapter17 where

import Control.Applicative
import Data.Monoid

-- ($) :: (a -> b) -> a -> b
-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

foo = [(*2), (*3)] <*> [4, 5]


-- :info (,)
-- data (,) a b = (,) a b 	-- Defined in ‘ghc-prim-0.5.0.0:GHC.Tuple’
-- ...
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’
-- ...
-- instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
-- instance Foldable ((,) a) -- Defined in ‘Data.Foldable’
-- instance Traversable ((,) a) -- Defined in ‘Data.Traversable’
-- instance (Monoid a, Monoid b) => Monoid (a, b)
--   -- Defined in ‘GHC.Base’

bar = ("woo", (+1)) <*> (" hoo!", 0 :: Int)
-- ("woo hoo!",1)

x = ((Sum 2), (+1)) <*> ((Sum 0), 0)
y = ((Product 3), (+9)) <*> ((Product 2), 8)
z = ((All True), (+1)) <*> ((All False), 0)

-- :t liftA
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- :t liftA2
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c



