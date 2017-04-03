{-# LANGUAGE FlexibleInstances #-}

module Chapter21 where

import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Foldable

class (Functor t, Foldable t) => Traversable' t where
  {-# MINIMAL traverse' | sequenceA' #-}
  -- ^^^ implement one or the other

  traverse' :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse' f = sequenceA' . fmap f

  sequenceA' :: Applicative f => t (f a) -> f (t a)
  sequenceA' = traverse' id

instance Traversable' [] where
  traverse' f (x:xs) = let
    fy = f x -- :: f b
    fty = traverse' f xs -- :: f (t b)
    in liftA2 (:) fy fty
  traverse' f [] = pure []

data Either' a b = Left' a | Right' b deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' x) = Right' $ f x

instance Applicative (Either' e) where
  pure = Right'
  Left' e <*> _ = Left' e
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _) = mempty
  foldMap f (Right' y) = f y

  foldr _ z (Left' _) = z
  foldr f z (Right' y) = f y z
  
instance Traversable' (Either' a) where
  traverse' _ (Left' x) = pure (Left' x)
  traverse' f (Right' y) = Right' <$> f y

instance Monad (Either' a) where
  return x = Right' x
  (>>=) (Right' x) func = func x
  (>>=) (Left' x) _ = Left' x

--instance Traversable' (Either a) where

-- duplicate with built-in instances
-- instance Functor ((,) a) where
--   fmap f (x,y) = (x, f y)

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) = (u `mappend` v, f x)

-- instance Foldable ((,) a) where
--   foldMap f (_, y) = f y
--   foldr f z (_, y) = f y 

instance Traversable' ((,) a) where
  traverse' f (x, y) = (,) x <$> f y

-- traverse' :: Applicative f => (a -> f b) -> t a -> f (t b)
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' = traverse' 

foo = undefined :: a -> Maybe b
xfoo = undefined :: [a]
xfoo' :: [Maybe a]
xfoo' = map foo xfoo'

xfoo'' :: Maybe [a]
xfoo'' = sequenceA xfoo'


f = undefined :: a -> Maybe b
xs = undefined :: [a]

-- :t map f xs 
-- map f xs :: [Maybe b]

-- :t sequenceA $ map f xs
-- sequenceA $ map f xs :: Maybe [a]

-- :t traverse f xs
-- traverse f xs :: Maybe [b]

-- :t (sequence .) . fmap 
-- (sequence .) . fmap
--   :: (Traversable t, Monad m) => (a1 -> m a) -> t a1 -> m (t a)


data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- string to object
decodeFn :: String -> Either' Err SomeObj
decodeFn = undefined

-- retrieve list of strings from hypothetical DB
fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined


-- clean this up
pipelineFn :: Query -> IO (Either' Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequenceA' (map decodeFn a) of
    (Left' err) -> return $ Left' $ Err
    (Right' res) -> do
      a <- makeIoOnlyObj res
      return $ Right' a


-- make instance
-- No instance for (Traversable' (Either Err))
-- and then use traverse'

-- successive improvements
pipelineFn' :: Query -> IO (Either' Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse' makeIoOnlyObj (mapM decodeFn a)

pipelineFn'' :: Query -> IO (Either' Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = (traverse' makeIoOnlyObj . mapM decodeFn =<<) . fetchFn

pipelineFn''' :: Query -> IO (Either' Err [(SomeObj, IoOnlyObj)])
pipelineFn''' = (traverse' makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

data Optional a = Nada | Yep a

instance Functor Optional where
  fmap f (Yep x) = Yep $ f x
  fmap _ Nada = Nada

instance Monoid (Optional a) where
  mappend Nada Nada = Nada
  mappend Nada (Yep x) = Yep x
  mappend (Yep x) Nada = Yep x
  mappend _ (Yep x) = Yep x

  mempty = Nada

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable' Optional where
  traverse' func Nada = pure Nada
  traverse' func (Yep x) = let
    fb = func x
    in Yep <$> fb

data List a = Nil | Cons a (List a) deriving Show

singleElementList x = Cons x Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x lx) = Cons (f x) (fmap f lx)

l1 = Cons 1 $ Cons 2 $ Cons 3 Nil
l2 = Cons 7 $ Cons 8 $ Cons 9 $ Cons 10 Nil

(+++) :: List a -> List a -> List a
(+++) Nil Nil = Nil
(+++) (Cons x Nil) tail = Cons x tail
(+++) (Cons x xs) tail = Cons x $ xs +++ tail
-- (+++) (Cons x Nil) tail@(Cons y ys) = (Cons x Nil) +++ tail
-- (+++) (Cons x xs) (Cons y ys) = Cons x (xs +++ (Cons y Nil) +++ ys)

l3 = l1 +++ l2

instance Monoid (List a) where
  mappend = (+++)
  mempty = Nil

instance Foldable List where
  foldr func y0 (Cons x Nil) = func x y0
  foldr func y0 (Cons x xs) = func x $ foldr func y0 xs
  foldr _ y0 Nil = y0

instance Traversable' List where
  traverse' _ Nil = pure Nil
  traverse' func (Cons x Nil) = fmap singleElementList $ func x
  traverse' func (Cons x xs) = let
    fys = traverse' func xs
    fy = func x
    in liftA2 Cons fy fys


data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right)
  fmap _ Empty = Empty

-- instance Monoid (Tree a) where
--   mappend Empty right@(Node _ _ _) = right
--   mappend left@(Node _ _ _) Empty = left
--   mappend Empty Empty = Empty
--   mappend left1@(Node left2 x right2) right1 =

instance Foldable Tree where
  foldMap func (Node left x right) =
    (foldMap func left) `mappend` (func x) `mappend` (foldMap func right)
  foldMap func (Leaf x) = func x
  foldMap _ Empty = mempty

instance Traversable' Tree where
  traverse' _ Empty = pure Empty
  traverse' func (Node left x right) =
    liftA3 (\l x r -> Node l x r) (traverse' func left) (func x) (traverse' func right)

-- 21.12

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable' Identity where
  traverse' f (Identity x) = fmap Identity (f x)


newtype Constant a b = Constant { getConstant :: a }

-- see page 661
-- FlipFunctor

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip Constant b) where
  fmap f (Flip constantA) = Flip $ Constant $ f (getConstant constantA)

-- "However, Flip Tuple a b is a distinct type from Tuple a b even if it’s only there to provide for different Functor instance behavior."

instance Foldable (Flip Constant b) where
  foldMap f (Flip constantA) = f (getConstant constantA)

instance Traversable' (Flip Constant b) where
  traverse' f (Flip constantA) = fmap (Flip . Constant) (f (getConstant constantA))


