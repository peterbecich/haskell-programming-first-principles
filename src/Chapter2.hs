module Chapter2 where

sayHello :: String -> IO ()
sayHello x = putStrLn $ "Hello, " ++ x ++ "!"

fooFunc :: Int -> Int -> Int
fooFunc x y = x+y

barFunc :: Int -> Int -> Int
barFunc x y = x `fooFunc` y

negative = 5 + (-9)

printInc n = print plusTwo
  where plusTwo = n + 2

foo x y = x `plus` y
  where plus a b = a + b

printInc' n = (\x -> print x) ( n + 2 )

six :: IO ()
six = printInc' 4
  
