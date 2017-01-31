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