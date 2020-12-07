backwardsInfinity = reverse [1..]

assignToGroups n aList = zip groups aList
    where groups = cycle [1..n]

--Q6.1
myRepeat val = cycle [val]

--Q6.2
subseq start stop list = take diff $ drop start list
    where diff = stop - start

--Q6.3
inFirstHalf item list = item `elem` firstHalf
    where firstHalf = take halfway list
          halfway = length list `div` 2