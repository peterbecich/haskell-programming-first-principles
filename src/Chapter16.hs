module Chapter16 where

a = fmap (+1) $ read "[1]" :: [Int]

b :: Maybe [String]
b = fmap (fmap (++ "lol")) (Just ["Hi, ", "Hello "])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        cast = fmap show ioi :: IO String
        prepended = fmap ("123"++) cast
        changed = fmap read prepended
    in fmap (*3) changed


