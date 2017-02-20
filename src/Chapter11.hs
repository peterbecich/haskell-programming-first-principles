module Chapter11 where

import Data.Functor
import Data.Int
import Data.Char
import Data.List
--import System.Random (getStdGen, randomRIO)
-- import Test.Hspec

data Bool' = False' | True'

-- data XY a = XY | a XY a
-- data [] a = [ ] | a : [a]

-- data XY a = [] | a : [a]

data PugType = PugData

-- :k PugType
-- PugType :: *

-- :k PugData 
-- <interactive>:1:1: error:
--     Not in scope: type constructor or class ‘PugData’
--     A data constructor of that name is in scope; did you mean DataKinds?

data Doggies a = Husky a | Mastiff a | NovaScotiaTollerRetriever a deriving (Eq, Show)

junior = NovaScotiaTollerRetriever 123

data Price = Price Integer deriving (Eq, Show)

-- Price 123
-- :t Price 
-- Price :: Integer -> Price

data CarManufacturer = Mini | Mazda | Tata | Nash deriving (Eq, Show)

-- :t CarManufacturer 
-- <interactive>:1:1: error:
--     Data constructor not in scope: CarManufacturer
-- :k CarManufacturer
-- CarManufacturer :: *

data Airline = PapuAir | AirAmerica | Hawaiian deriving (Eq, Show)

data Vehicle = Car CarManufacturer Price | Plane Airline deriving (Eq, Show)

myCar = Car Nash $ Price 2000
otherCar = Car Mini $ Price 3000

-- 2: cardinality is three
-- 3: cardinality is 65536 or 2^16

data Goats = Goats Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats numberOfGoats)
  | numberOfGoats > 5 = True
  | otherwise = False

newtype Llamas = Llamas Int deriving (Eq, Show)

tooManyLlamas :: Llamas -> Bool
tooManyLlamas (Llamas numberOfLlamas)
  | numberOfLlamas > 10 = True
  | otherwise = False

data Color = Blue | Green | White | Yellow deriving (Eq, Show)

-- class Bird b

-- class Bird b where
--   color :: Color

newtype Budgerigar = Budgerigar Color deriving (Eq, Show)

newtype Cockatiel = Cockatiel { color :: Color } deriving (Eq, Show)

-- instance Bird (Budgerigar b) where
--   color = b
-- instance Bird Cockatiel

bossy = Budgerigar Yellow

sparky = Cockatiel White
sparky' = Cockatiel { color = White }

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Goats where
  tooMany (Goats numberOfGoats)
    | numberOfGoats > 5 = True
    | otherwise = False

instance TooMany Llamas where
  tooMany (Llamas numberOfLlamas)
    | numberOfLlamas > 10 = True
    | otherwise = False
-- sum type
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
-- cardinality is 4

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
-- cardinality is (cardinality of Int8) + (cardinality of Bool)

-- cardinality is 3
data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)

-- product type
-- cardinality is 9
data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

quantumStates = [MkTwoQs a b | a <- [QuantumTrue, QuantumFalse, QuantumBoth], b <- [QuantumTrue, QuantumFalse, QuantumBoth]]

data Person = Person { name :: String, age :: Int } deriving (Eq, Show)

peter = Person { name = "Peter", age = 24 }


data Expr =
  Number Int
  | Add Expr Expr
  | Minus Expr Expr
  | Mult Expr Expr
  | Divide Expr Expr

fortyTwoExpr :: Expr
fortyTwoExpr = Mult (Add (Number 3) (Number 4)) (Number 6)

reduceExpr :: Expr -> Int
reduceExpr (Number n) = n
reduceExpr (Add x y) = reduceExpr x + reduceExpr y
reduceExpr (Minus x y) = reduceExpr x - reduceExpr y
reduceExpr (Mult x y) = reduceExpr x * reduceExpr y
reduceExpr (Divide x y) = reduceExpr x `div` reduceExpr y

fortyTwo = reduceExpr fortyTwoExpr

-- :k [Int]
-- [Int] :: *
-- :k []
-- [] :: * -> *

-- rolls over
-- twoToTheEighthMinusOne = 2 * (maxBound :: Int8)

-- twoToTheEighthMinusOne = -1*(minBound :: Int8) + (maxBound :: Int8)

data Example = MakeExample deriving Show

-- :info Example 
-- data Example = MakeExample
--  Defined at /Users/peterbecich/haskell/haskell-programming-first-principles/.stack-work/intero/intero70406zph.hs:148:1
-- instance [safe] Show Example


data OperatingSystem = GnuPlusLinux | OpenBSD | Mac | Hurd | Windows deriving (Eq, Show)

data ProgrammingLanguage = Haskell | C | Agda deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem, lang :: ProgrammingLanguage } deriving (Eq, Show)

-- :t Programmer 
-- Programmer :: OperatingSystem -> ProgrammingLanguage -> Programmer

-- not the right way to partially apply
-- macProgrammer = Programmer { os = Mac }

data Silly a b c d = MakeSilly a b c d deriving Show
-- :k Silly 
-- Silly :: * -> * -> * -> * -> *

data Goofy b c d = Silly Int8 b c d

-- :k Goofy 
-- Goofy :: * -> * -> * -> *

-- quadruple
-- :k (,,,)
-- (,,,) :: * -> * -> * -> * -> *

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b t@(Node left a right)
  | b == a = t
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)


t1 = insert' 67 Leaf
t2 = insert' 75 t1
t3 = insert' 80 t2
t4 = insert' (65) t3

instance Functor BinaryTree where
  fmap _ Leaf = Leaf
  fmap f (Node left x right) = Node (f <$> left) (f x) (f <$> right)

letters :: BinaryTree Char
letters = chr <$> t4

-- :t Node 
-- Node :: BinaryTree a -> a -> BinaryTree a -> BinaryTree a

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = x : (preorder left) ++ (preorder right)

lettersList = preorder letters

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = (preorder left) ++ x : (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = (preorder left) ++ (preorder right) ++ [x]


buildTree :: (Ord a) => [a] -> BinaryTree a
buildTree list = foldr insert' Leaf list

letterTree = buildTree ['A'..'Z']



testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "Preorder fine!"
  else putStrLn "Preorder broken"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine"
  else putStrLn "Inorder broken"

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine"
  else putStrLn "Postorder broken"

testTraversals :: IO ()
testTraversals = testPreorder >> testInorder >> testPostorder


