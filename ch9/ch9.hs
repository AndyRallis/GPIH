import Data.Char

myRemove _ []       = []
myRemove f (x : xs) = if f x then myRemove f xs else x : myRemove f xs

myProduct :: (Foldable t, Num b) => t b -> b
myProduct = foldl (*) 1

--Q9.1
myElem k xs = length matchingElements > 0
  where matchingElements = filter (== k) xs

--Q9.2
newIsPalindrome xs = original == reversed
 where
  original = (map toLower . filter (/= ' ')) xs
  reversed = reverse original

--Q9.3
harmonic n = foldr (\x y -> y + 1/x) 0 [1..n]
