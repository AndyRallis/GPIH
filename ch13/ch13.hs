--Q13.1 The max bound is different

--Q13.2 
inc :: Int -> Int
inc x = x + 1

highestInt :: Int
highestInt = maxBound :: Int

-- inc spills over the bound and goes negative at the maxBound, succ throws an exception

--Q13.3
cycleSucc :: (Bounded a, Enum a, Ord a) => a -> a
cycleSucc n | n == maxBound = minBound
            | otherwise     = succ n
