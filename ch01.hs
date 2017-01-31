prod [] = 1
prod (n:ns) = n * prod ns

qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
                where
                    smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b >= x]

