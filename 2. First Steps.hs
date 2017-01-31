-- 2.1 Glasgow Haskell Compiler

-- 2.3 Standard Prelude

-- 2.4 Function Application

-- 2.5 Haskell Scripts
-- naming requirements, layout rule, tabs, comments

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

-- Exercises
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]