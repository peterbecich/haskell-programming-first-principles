module Chapter21 where

import Control.Monad
import Control.Applicative

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

-- traverse' :: Applicative f => (a -> f b) -> t a -> f (t b)
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' = traverse' 

foo = undefined :: a -> Maybe b
xfoo = undefined :: [a]
xfoo' :: [Maybe a]
xfoo' = map foo xfoo'

xfoo'' :: Maybe [a]
xfoo'' = sequenceA xfoo'

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- string to object
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- retrieve list of strings from hypothetical DB
fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined


-- clean this up
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ Err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a


-- make instance
-- No instance for (Traversable' (Either Err))
-- and then use traverse'

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn

pipelineFn''' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn''' = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

