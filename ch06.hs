-- 6.1 Basic Concepts
fac :: Int -> Int
fac n = product [1..n]

fac2 :: Int -> Int
fac2 0 = 1
fac2 n = n * fac2(n-1)

-- 6.2 Recursion on List
rproduct :: Num a => [a] -> a
rproduct [] = 1
rproduct (n:ns) = n * product ns

rlength :: [a] -> Int
rlength [] = 0
rlength (_:ns) = 1 + rlength ns

rreverse :: [a] -> [a]
rreverse [] = []
rreverse (n:ns) = rreverse ns ++ [n]

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort   [] = []
isort   (x:xs) = insert x (isort xs)

-- 6.3 Multiple Arguments
rzip :: [a] -> [b] -> [(a,b)]
rzip [] _ = []
rzip _ [] = []
rzip (x:xs) (y:ys) = (x,y) : rzip xs ys 

-- 6.4 Multiple Recursion
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- 6.5 Mutual Recursion
reven :: Int -> Bool
reven 0 = True
reven n = rodd (n-1)

rodd :: Int -> Bool
rodd 0 = False
rodd n = reven (n-1)

revens :: [a] -> [a]
revens [] = []
revens (x:xs) = x : rodds xs

rodds :: [a] -> [a]
rodds [] = []
rodds (_:xs) = revens xs

-- 6.6 Advice on Recursion
rdrop :: Integral b => b -> [a] -> [a]
rdrop 0 xs       = xs 
rdrop _ []       = []
rdrop n (_:xs)   = rdrop (n-1) xs

rinit :: [a] -> [a]
rinit [_] = []
rinit (x:xs) = x : rinit xs

-- Exercises
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

rexp :: Int -> Int -> Int
_ `rexp` 0 = 1
n `rexp` m = n * (n `rexp` (m-1))

euclid :: Int -> Int -> Int
euclid n m | n == m = n
           | n > m  = euclid (n-m) m
           | n < m  = euclid n (m-n)

rand :: [Bool] -> Bool
rand [] = True
rand (x:xs) = x && (rand xs)

rconcat :: [[a]] -> [a]
rconcat [] = []
rconcat (x:xs) = x ++ rconcat xs

rreplicate :: Int -> a -> [a]
rreplicate 0 x = []
rreplicate n x = x : rreplicate (n-1) x

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

relem :: Eq a => a -> [a] -> Bool
relem x [] = False
relem x (y:ys)  | x == y      = True
                | otherwise   = relem x ys 

rmerge :: Ord a => [a] -> [a] -> [a]
rmerge [] ys    = ys
rmerge xs []    = xs
rmerge (x:xs) (y:ys)    | x <= y    = x : rmerge xs (y:ys)
                        | y < x     = y : rmerge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
            where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []    = []
msort [x]   = [x]
msort xs    = rmerge (msort (fst (halve xs))) (msort (snd (halve xs)))