half :: Integer -> Integer
half n = n `div` 2

printDouble :: Int -> String
printDouble n = show $ n * 2

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n then f n else n

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x, y, z)

--Q11.1
-- filter type signature (a->bool) -> [a] -> [a]

--Q11.2 cannot because head :: a and tail :: [a]

--Q11.3
-- type class is myFoldl :: (b-> a -> b) -> b -> [a] -> b
-- specifically for a list not a generic container

