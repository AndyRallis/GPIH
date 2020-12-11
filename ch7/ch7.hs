myGcd a b = if remainder == 0 then b else myGcd b remainder
  where remainder = a `mod` b

myEmpty []    = True
myEmpty _list = False

myTail1 (_x : xs) = xs

--Q7.1
myTail []        = []
myTail (_x : xs) = xs

--Q7.2
myGcd2 a 0 = a
myGcd2 a b = myGcd2 b (a `mod` b)
