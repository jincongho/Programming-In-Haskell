-- 1.1 Functions

-- 1.2 Functional Programming

-- 1.3 Features

-- 1.4 Historical Backgrounds

-- 1.5 A Taste of Haskell

-- Exercises
prod [] = 1
prod (n:ns) = n * prod ns

qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
                where
                    smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b >= x]

