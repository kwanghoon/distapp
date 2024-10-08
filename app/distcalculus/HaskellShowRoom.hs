module HaskellShowRoom where

type Greeting = String

greet :: Show a => Greeting -> a -> String
greet greeting who = greeting ++ " " ++ show who

helloWorld :: IO ()
helloWorld =
  putStrLn "What is your name?" >>= (\_    ->
  getLine)                      >>= (\name ->
  putStrLn "How to greet you?"  >>= (\_    ->
  getLine)                      >>= (\g    ->
  putStrLn $ greet g name))