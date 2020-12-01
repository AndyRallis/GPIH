calcChange owed given = if change > 0
                        then change
                        else 0
    where change = given - owed

doublePlusTwo x = doubleX + 2
    where doubleX = x * 2

inc n = n + 1

double n = n * 2

square x = x**2

weirdReturn x = if even x
                then x - 2
                else 3 * x + 1