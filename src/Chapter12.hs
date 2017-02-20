module Chapter12 where

type Name = String
type Age = Int
data Person = Person Name Age deriving Show

-- :k Either 
-- Either :: * -> * -> *



-- :k Maybe ( Maybe Char )
-- Maybe ( Maybe Char ) :: *

-- :k Maybe Int 
-- Maybe Int :: *

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

data UnaryC = UnaryC Int deriving Show


