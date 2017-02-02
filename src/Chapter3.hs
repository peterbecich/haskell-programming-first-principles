module Chapter3 where

hello :: IO ()
hello = putStrLn "hello"

helloThere :: IO ()
helloThere = putStrLn s
  where s = "hello "++"there"

helloThere' :: IO ()
helloThere' = let s = "hello "++"there"
  in putStrLn s

-- helloThere'' :: IO ()
-- helloThere'' = do
--   putStrLn "hello "++"there"
--   where unused = 1+1

ht = (++) "hello " "there"
ht' = "hello " ++ "there"

two = tail [1..10]
printTwoToTen = putStrLn $ show two
