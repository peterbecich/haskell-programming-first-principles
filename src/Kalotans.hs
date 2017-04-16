module Kalotans where

-- https://wiki.haskell.org/All_About_Monads#Anatomy_of_a_monad_transformer

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State

type Var = String
type Value = String
data Predicate = Is Var Value -- var has specific value
               | Equal Var Var -- vars have the same unspecified value
               | And Predicate Predicate  -- both are True
               | Or Predicate Predicate -- one or both are True
               | Not Predicate
  deriving (Eq, Show)

type Variables = [(Var, Value)]

isNot :: Var -> Value -> Predicate
isNot var value = Not (Is var value)

-- if a is true, then b must also be true
implies :: Predicate -> Predicate -> Predicate
implies p1 p2 = Not (And p1 (Not p2))

-- exclusive Or
orElse :: Predicate -> Predicate -> Predicate
orElse p1 p2 = Or (And p1 (Not p2)) (And (Not p1) p2)

-- check a predicate with the given variable bindings
check :: Predicate -> Variables -> Maybe Bool
check (Is var value) vars = do
  val <- lookup var vars
  return (val == value)
check (Equal v1 v2) vars = do
  val1 <- lookup v1 vars
  val2 <- lookup v2 vars
  return (val1 == val2)
check (And p1 p2) vars = liftM2 (&&) (check p1 vars) (check p2 vars)
check (Or p1 p2) vars = liftM2 (||) (check p1 vars) (check p2 vars)
check (Not p) vars = liftM (not) (check p vars)


-- code for representing and solving constraint satisfaction problems

data ProblemState = PS { vars :: Variables, constraints :: [Predicate] }

type NonDeterministicState a = StateT ProblemState [] a

-- :t get 
-- get :: Monad m => StateT s m s
-- :t gets
-- gets :: Monad m => (s -> a) -> StateT s m a

getVar :: Var -> NonDeterministicState (Maybe Value)
getVar var = do
  vrs <- gets vars
  return $ lookup var vrs

-- (123 /=) :: (Num a, Eq a) => a -> Bool
setVar :: Var -> Value -> NonDeterministicState ()
setVar var value = do
  state <- get
  vrs' <- return $ filter ((var /=) . fst) (vars state)
  put $ state { vars = (var, value) : vrs' }

isConsistent :: Bool -> NonDeterministicState Bool
isConsistent b = do
  cnstrnts <- gets constraints
  vrs <- gets vars
  let results = fmap (\pred -> check pred vrs) cnstrnts
  return $ and (fmap (maybe b id) results)

getFinalVars :: NonDeterministicState Variables
getFinalVars = do
  cnsistnt <- isConsistent False
  _ <- guard cnsistnt
  gets vars

getSolution :: NonDeterministicState a -> ProblemState -> Maybe a
getSolution nds state = listToMaybe (evalStateT nds state)

getAllSolutions :: NonDeterministicState a -> ProblemState -> [a]
getAllSolutions nds state = evalStateT nds state





