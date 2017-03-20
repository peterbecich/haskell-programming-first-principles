module NaiveLensExamples where

-- http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html

data User = User { name :: String, age :: Int } deriving Show
data Project = Project { owner :: User, value :: Int } deriving Show

bob = User { name = "Bob", age = 30 }
project1 = Project { owner = bob, value = 1 }
alice = bob { name = "Alice" }
project2 = project1 { owner = alice, value = 2 }

data NaiveLens s a = NaiveLens
  { view :: s -> a
  , set :: a -> s -> s
  }

nameLens :: NaiveLens User String
nameLens = NaiveLens (\user -> name user) (\newName user -> user { name = newName })
ageLens :: NaiveLens User Int
ageLens = NaiveLens (\user -> age user) (\newAge user -> user { age = newAge })

robert = set nameLens "Robert" bob

changeOwnership :: NaiveLens Project User
changeOwnership =
  NaiveLens (\project -> owner project) (\newOwner project -> project { owner = newOwner })

--changeOwnershipAgeSet :: NaiveLens Project Int
changeOwnershipAgeSet proj newAge = let
  newOwner = set ageLens newAge (owner proj)
  in set changeOwnership newOwner proj

-- changeOwnershipAgeLens :: NaiveLens Project Int
-- changeOwnershipAgeLens = 


