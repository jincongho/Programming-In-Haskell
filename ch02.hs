double x = x + x
quadruple x = double (double x)
-- factorial of a positive integer
factorial n = product [1..n]
{-
    avarage [1..4]
-}
average ns = sum ns `div` length ns
a = b + c
    where 
        b = 1
        c = 2
d = a * 2
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]