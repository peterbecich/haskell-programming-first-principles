
module Chapter11 where

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

data NumberOrBool = Numba Int | BoolyBool Bool deriving (Eq, Show)
-- cardinality is (cardinality of Int) + (cardinality of Bool)

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

