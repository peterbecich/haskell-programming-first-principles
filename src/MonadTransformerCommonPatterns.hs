{-# LANGUAGE FlexibleContexts #-}

module MonadTransformerCommonPatterns where

-- http://book.realworldhaskell.org/read/monad-transformers.html#id657356

import Control.Monad.Reader

myName step = do
  name <- ask
  return (step ++ ", my name is " ++ name)

-- local :: MonadReader r m => (r -> r) -> m a -> m a

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)

localExample' = runReader localExample

-- localExample' "foo"
-- ("First, my name is foo","Second, my name is foody","Third, my name is foo")

