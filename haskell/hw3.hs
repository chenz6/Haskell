-------------------------------------------------------------------------
-- Group members：Qiulin Zhang, Zhuoling Chen, Sangyeon Lee, Ingyu Woo  
-- CS 381, Spring 2018                                                  
-- Homework 3 (Types)                                        
-- Due：May 15, 2018，2pm                                             
--------------------------------------------------------------------------

module HW3 where

{-
    Exercise 1. A Rank-Based Type Systems for the Stack Language
-}
type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC
         | SWAP
         | POP Int
         deriving Show

-- 1. (a)
--
type Rank    = Int
type CmdRank = (Int, Int)

rankC :: Cmd -> CmdRank
rankC (LD _)  = (0, 1)
rankC (ADD)   = (2, 1)
rankC (MULT)  = (2, 1)
rankC (DUP)   = (1, 2)
rankC (INC)   = (1, 1)
rankC (SWAP)  = (2, 2)
rankC (POP x) = (x, 0)

rank :: Prog -> Rank -> Maybe Rank
rank [] r     = Just r
rank (x:xr) r = case ( (input <= r)) of
                True  -> (rank xr (r - input + output))
                False -> Nothing
                where (input, output) = (rankC x)

rankP :: Prog -> Maybe Rank
rankP p = rank p 0

test11 = rankP [(LD 1)]                    -- Just 1
test12 = rankP [(LD 1), (LD 1), ADD]       -- Just 1
test13 = rankP [(LD 1), (LD 1), ADD, MULT] -- Nothing

-- 1. (b)
--
type Stack = [Int]

semCmd :: Cmd -> Stack -> Stack
semCmd (LD i)  s        = i:s
semCmd ADD     (x:y:xr) = (x+y):xr
semCmd MULT    (x:y:xr) = (x*y):xr
semCmd DUP     (x:xr)   = x:x:xr
semCmd INC     (x:xr)   = (x+1):xr
semCmd SWAP    (x:y:xr) = y:x:xr
semCmd (POP n) (x:xr)   | (n == 0) = (x:xr)
                        | otherwise = semCmd (POP (n-1)) xr

sem :: Prog -> Stack -> Stack
sem [] s = s
sem (c:cr) s = sem cr (semCmd c s)

semStatTC :: Prog -> Maybe Stack
semStatTC p = case (rankP p) of
              Nothing   -> Nothing
              otherwise -> Just (sem p [])
-- 
-- The function “sem” can be simplified because it no longer needs 
-- to check its input type since the type checking precedes
-- the invocation of the function.
--

{-
    Exercise 2. Shape Language
-}
data Shape = X
           | TD Shape Shape
           | LR Shape Shape
           deriving Show

type BBox = (Int, Int)

-- 2. (a)
--
bbox :: Shape -> BBox
bbox X = (1, 1)
bbox (TD s1 s2) = (max w1 w2, h1 + h2)
                   where (w1, h1) = bbox s1
                         (w2, h2) = bbox s2
bbox (LR s1 s2) = (w1 + w2, max h1 h2)
                   where (w1, h1) = bbox s1
                         (w2, h2) = bbox s2

test21 = bbox X               -- (1,1)
test22 = bbox (TD X X)        -- (1,2)
test23 = bbox (LR X (TD X X)) -- (2,2)

-- 2. (b)
--
rect :: Shape -> Maybe BBox
rect X          = Just (1, 1)
rect (TD s1 s2) | w1 == w2  = Just (w1, h1 + h2)
                | otherwise = Nothing
                where (w1, h1) = bbox s1
                      (w2, h2) = bbox s2
rect (LR s1 s2) | h1 == h2  = Just (w1 + w2, h1)
                | otherwise = Nothing
                where (w1, h1) = bbox s1
                      (w2, h2) = bbox s2

test24 = rect X               -- Just (1,1)
test25 = rect (TD X X)        -- Just (1,2)
test26 = rect (LR X (TD X X)) -- Nothing


{-
    Exercise 3. Parametric Polymorphism
-}

-- 3. (a)
{-
    f x y = if null x then [y] else x

    g x y = if not (null x) then [] else [y]
    g [] y = []
-}

-- 3. (a) (1)
--
-- f :  [a] -> a -> [a]
-- g :  [a] -> b -> [b]

-- 3. (a) (2)
--
-- The 'null' function requires a list as an input, and the element of 'x'
-- shoulde be same as 'y' in order to match the result type for the function 'f'.
-- Therefore, 'x' should be [a], and 'y' should be a.
--
-- However, for the function 'g', although 'x' should be a list, it should not be
-- relevant to 'y' because the result types of this function is related to 'y' only.
-- Therefore, 'x' should be [a], and 'y' should be b.

-- 3. (a) (3)
--
-- The type of function 'g' is more general. Because the function 'f' requires 
-- the same type of 'x' and 'y'. However, the function 'g' is more flexible 
-- because it does not require the same type of x and y.

-- 3. (a) (4)
--
-- As mentioned in 3. (a) (2), in the function 'f', 'x' and '[y]' appear after "then"
-- and "else", so they need to be the same type. However, in function 'g', '[]' and
-- '[y]',appear after "then" and "else", so the element of 'x' cannot limit the type
-- of 'y'.


-- 3. (b)
-- h :: [b] -> [(a, b)] -> [b]
--
h x y = map snd y ++ x


-- 3. (c)
-- k :: (a -> b) -> ((a -> b) -> a) -> b
--
k x y = (x (y x))


-- 3. (d)
-- a -> b ?
--
-- No, although "t a = t a" can show "a -> b" in the ghci, it will be stuck in 
-- infinity loop in runtime since it calls itself and it doesn't have the base case 
-- which can terminate this program. Therefore, defining a function of type a -> b 
-- is not possible.

