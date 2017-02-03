-- 7.1 Basic Concepts
twice :: (a -> a) -> a -> a
twice f x       = f (f x)

twice_mul       = twice (*2) 2
twice_reverse   = twice reverse [1..10]
-- @TODO: curried form?

-- 7.2 Processing Lists
hmap :: (a -> b) -> [a] -> [b]
hmap f xs       = [f x | x <- xs]

hrmap :: (a -> b) -> [a] -> [b]
hrmap f []      = []
hrmap f (x:xs)  = f x : hrmap f xs

hrmap_increment = hmap (+1) [1..10]
hrmap_even      = hmap even [1..10]
hrmap_reverse   = hmap reverse ["haskell", "is", "fun"]

hfilter :: (a -> Bool) -> [a] -> [a]
hfilter p xs    = [x | x <- xs, p x]

hrfilter :: (a -> Bool) -> [a] -> [a]
hrfilter p []   = []
hrfilter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

hrfilter_even   = hrfilter even [1..10]
hrfilter_large  = hrfilter (>5) [1..10]
hrfilter_not    = hrfilter (/= ' ') "haskell is fun"

sumsqreven :: [Int] -> Int
sumsqreven ns   = sum (map (^2) (filter even ns)) 

sumsqreven_exp  = sumsqreven [1..20]

all_even        = all even [1..20]
any_odd         = any odd [1..20]
take_while_even = takeWhile even [10..20]
drop_while_even = dropWhile even [10..20]

-- 7.3 foldr function
hfoldr :: (a -> b -> b) -> b -> [a] -> b
hfoldr f v []   = v
hfoldr f v (x:xs)   = f x (hfoldr f v xs)

hfsum :: Num a => [a] -> a
hfsum           = hfoldr (+) 0

hfproduct :: Num a => [a] -> a
hfproduct       = hfoldr (*) 1

hfor :: [Bool] -> Bool
hfor            = hfoldr (||) False

hfand :: [Bool] -> Bool
hfand           = hfoldr (&&) True

hflength :: [a] -> Int
hflength        = hfoldr (\_ n -> 1+n) 0

hfreverse :: [a] -> [a]
hfreverse       = foldr (\x xs -> xs ++ [x]) []

-- 7.4 foldl function
hfoldl :: (a -> b -> a) -> a -> [b] -> a
hfoldl f v []   = v
hfoldl f v (x:xs)   = foldl f (f v x) xs 

-- 7.5 Composition Operator
comp :: (b -> c) -> (a -> b) -> (a -> c)
f `comp` g = \x -> f (g x)

hodd            = not `comp` even
htwice f        = f `comp` f
hsumsqreven     = sum `comp` map (^2) `comp` filter even

-- Exercises
hall :: (a -> Bool) -> [a] -> Bool
-- hall f xs       = hfand [f x | x <- xs]
hall f          = and . map f

hany :: (a -> Bool) -> [a] -> Bool
-- hany f xs       = hfor [f x | x <- xs]
hany f          = or . map f

htakeWhile :: (a -> Bool) -> [a] -> [a]
htakeWhile _ [] = [] 
htakeWhile f (x:xs) 
    | f x       = x : htakeWhile f xs
    | otherwise = [] 

hdropWhile :: (a -> Bool) -> [a] -> [a]
hdropWhile _ [] = []
hdropWhile f (x:xs)
    | f x       = hdropWhile f xs
    | otherwise = x : xs

hfmap :: (a -> b) -> [a] -> [b]
hfmap f         = hfoldr (\x xs -> f x : xs) []

hffilter :: (a -> Bool) -> [a] -> [a]
hffilter f      = hfoldr (\x xs -> if f x then x:xs else xs) 

dec2int :: [Int] -> Int
dec2int         = hfoldl (\x y -> 10*x + y) 0

