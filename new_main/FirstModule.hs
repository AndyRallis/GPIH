module FirstModule where

helloFunction name = "Hello " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Enter your name"
  name <- getLine
  let message = helloFunction name
  putStrLn message
