--Q8.1
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

--Q8.2
-- This is essentially converted to a loop to avoid double recursion
fastFib _ _ 0 = 0
fastFib _ y 1 = y
fastFib x y counter = fastFib (x+y) x (counter - 1)
