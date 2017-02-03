-- Tautology Checker
-- after thought: possible to do this analytically by using logical equivalence? faster?

data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop

-- value lookup table
type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- return variables in prop
vars :: Prop -> [Char]
vars (Const _)      = []
vars (Var x)        = [x]
vars (Not p)        = vars p
vars (And p q)      = vars p ++ vars q
vars (Imply p q)    = vars p ++ vars q

-- produce lists of logical values

type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0   = []
int2bin n   = n `mod` 2 : int2bin (n `div` 2)

{- bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
        where 
            range       = [0..(2^n)-1]
            make n bs   = take n (bs ++ repeat 0)
            conv 0      = False
            conv 1      = True -}

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
        where bss = bools (n-1)

-- substitute variables with logical values
rmdups :: Eq a => [a] -> [a]
rmdups []   = []
rmdups (x:xs) = x : filter(/=x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs)) 
            where vs = rmdups (vars p)

-- evalutes prop given substitution

eval :: Subst -> Prop -> Bool
eval _ (Const b)    = b
eval s (Var x)      = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Imply p q)  = eval s p <= eval s q

-- main interface
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')