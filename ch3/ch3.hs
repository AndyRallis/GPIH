-- Basic where
sumSquareOrSquareSum x y = if sumSquare > squareSum
                           then sumSquare
                           else squareSum
    where sumSquare = x^2 + y^2
          squareSum = (x + y)^2

-- Rewrite for my own where
sumSquareOrSquareSumCustomWhere x y = (\sumSquare squareSum -> 
                           if sumSquare > squareSum
                           then sumSquare
                           else squareSum) (x^2 + y^2) (x + y)^2

-- Lambda version of where
doubleDouble x = (\dubs -> dubs * 2) (x * 2)

-- Rewrite where with let
sumSquareOrSquareSumLet x y = let sumSquare = x^2 + y^2
                                  squareSum = (x + y)^2
                              in
                                if sumSquare > squareSum
                                then sumSquare
                                else squareSum

-- overwrite with lambdas
loverwrite x = (\x -> 
                  (\x -> 
                        (\x -> x) 4
                   ) 3
                  ) 2

-- 3.2
counterLet x = let x = x + 1
               in
                 let x = x + 1
                 in x

counter x = (\x -> 
             (\x-> x) (x+1)
            ) (x+1)