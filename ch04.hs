-- 4.1 New from Old
ev :: Integral a => a -> Bool
ev n = n `mod` 2 == 0

sAt :: Int -> [a] -> ([a], [a])
sAt n xs = (take n xs, drop n xs)

rec :: Fractional a => a -> a
rec n = 1 / n

-- 4.2 Conditional Expressions
ab :: Int -> Int
ab n = if n >= 0 then n else -n

sn :: Int -> Int
sn n = if n < 0 then -1 else
        if n == 0 then 0 else 1

-- 4.3 Guarded Equations
abt :: Int -> Int
abt n | n >= 0      = n
      | otherwise   = -n 

snt :: Int -> Int
snt n | n < 0       = -1
      | n == 0      = 0
      | n > 0       = 1

-- 4.4 Pattern Matching
nt :: Bool -> Bool
nt False = True
nt True = False

(&) :: Bool -> Bool -> Bool
-- True & True = True
-- _ & _ = False
True & b = b
False & _ = False

fsta :: (a,b) -> a
fsta (x,_) = x

snda :: (a,b) -> b
snda (_,y) = y

test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

testt :: [Char] -> Bool
testt ('a':_) = True
testt _ = False

heada :: [a] -> a
heada (x:_) = x

taila :: [a] -> [a]
taila (_:xs) = xs

-- 4.5 Lambda Expression
addl :: Int -> (Int -> Int)
addl = \x -> (\y -> x + y)

constl :: a -> (b -> a)
constl x = \_ -> x

oddn :: Int -> [Int]
oddn n = map f [0..n-1]
        where f x = x*2+1

oddnl :: Int -> [Int]
oddnl n = map (\x -> 2*x+1) [0..n-1]

-- Exercises
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
            where n = length xs `div` 2

halvet :: [a] -> ([a], [a])
halvet xs = sAt (length xs `div` 2) xs

third :: [a] -> a
third xs = head (tail (tail xs))

thirdb :: [a] -> a
thirdb xs = xs !! 2

thirdc :: [a] -> a
thirdc [_,_,x,_] = x

safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

safetailb :: [a] -> [a]
safetailb xs | null xs      = xs
             | otherwise    = tail xs

safetailc :: [a] -> [a]
safetailc [] = []
safetailc (_:xs) = xs

(|||) :: Bool -> (Bool -> Bool)
True ||| _ = True
_ ||| _ = False

multl :: Int -> (Int -> (Int -> Int))
multl = \x -> (\y -> (\z -> x*y*z))

luhnDouble :: Int -> Int
luhnDouble n = if x > 9 then x-9 else x
                where x = n*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | x `mod` 10 == 0  = True
             | otherwise        = False
             where x = (luhnDouble a) + b + (luhnDouble c) + d