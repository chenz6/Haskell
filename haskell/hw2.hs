-------------------------------------------------------------------------
-- Group members：Qiulin Zhang, Zhuoling Chen, Sangyeon Lee, Ingyu Woo  
-- CS 381, Spring 2018                                                  
-- Homework 2 (Semantics)                                        
-- Due：May 3, 2018，2pm                                             
--------------------------------------------------------------------------

module HW2 where

{-
    Exercise 1. A Stack Language

    S ::=C|C;S
    C ::=LDInt |ADD|MULT|DUP
-}

type Prog  = [Cmd]

data Cmd   = LD Int
           | ADD
           | MULT
           | DUP
           --- Ex 2. (a) ---
           | DEF String Prog
           | CALL String
           -----------------
           deriving Show

type Stack = [Int]

type D = Maybe Stack -> Maybe Stack

sem :: Prog -> D
sem []     (Just s) = Just s
sem (x:xs) (Just s) = case sem xs (semCmd x (Just s)) of
                      Nothing -> Nothing
                      Just s  -> Just s
sem _      _        = Nothing

semCmd :: Cmd -> D
semCmd (LD i) (Just s) = Just (i:s)
semCmd ADD    (Just s) | isLenEnough (Just s) 2 == True = Just ((sum (take 2 s)):drop 2 s)
                       | otherwise = Nothing
semCmd MULT   (Just s) | isLenEnough (Just s) 2 == True = Just ((product (take 2 s)):drop 2 s)
                       | otherwise = Nothing
semCmd DUP    (Just s) | isLenEnough (Just s) 1 == True = Just ((head s):s)
                       | otherwise = Nothing

isLenEnough :: Maybe Stack -> Int -> Bool
isLenEnough (Just s) i | length s >= i = True
                       | otherwise = False

emptyList = Just []

testCmd1 = [LD 3,DUP,ADD,DUP,MULT]
testCmd2 = [LD 3,ADD]
testCmd3 = []

t1 = sem testCmd1 emptyList
t2 = sem testCmd2 emptyList
t3 = sem testCmd3 emptyList


{-
    Exercise 2. Extending the Stack Language by Macros

    C ::=LDInt |ADD|MULT|DUP|DEFString(S)|CALLString
-}

-- 2. (a)
--
-- Look at the above abstract syntax

-- 2. (b)
--
type Macros = [(String, Prog)]
type State2 = (Macros, Stack)

-- 2. (c)
--
type T = Maybe State2 -> Maybe State2

sem2 :: Prog -> T
sem2 []     (Just (m, s)) = Just (m, s)
sem2 (x:xs) (Just (m, s)) = case sem2 xs (semCmd2 x (Just (m, s))) of
                            Nothing -> Nothing
                            Just (m, s)  -> Just (m, s)
sem2 _      _             = Nothing

semCmd2 :: Cmd -> T
semCmd2 (LD i)  (Just (m, s)) = Just (m, (i:s))
semCmd2 ADD     (Just (m, s)) | isLenEnough (Just s) 2 == True = Just (m, (sum (take 2 s)):drop 2 s)
                              | otherwise = Nothing
semCmd2 MULT    (Just (m, s)) | isLenEnough (Just s) 2 == True = Just (m, (product (take 2 s)):drop 2 s)
                              | otherwise = Nothing
semCmd2 DUP     (Just (m, s)) | isLenEnough (Just s) 1 == True = Just (m, (head s):s)
                              | otherwise = Nothing
semCmd2 (DEF str cmds) (Just (m, s)) = Just ((str, cmds):m, s)
semCmd2 (CALL str)     (Just (m, s)) = case lookup str m of
                                       Nothing -> Nothing
                                       Just p  -> sem2 p (Just (m, s))

emptyState2 = Just ([], [])

testCmd21 = [LD 5, DEF "m1" [DUP, ADD, DUP, MULT], DEF "m2" [DUP, MULT], CALL "m1", CALL "m2"]

t21 = sem2 testCmd21 emptyState2


{-
    Exercise 3. Mini Logo
-}

data Cmd3  = Pen Mode
           | MoveTo Int Int
           | Seq Cmd3 Cmd3
           deriving Show

data Mode  = Up | Down
             deriving Show

type State = (Mode, Int, Int)
type Line  = (Int, Int, Int, Int)
type Lines = [Line]

--semS :: Cmd3 -> State -> (State, Lines)
--
semS :: Cmd3 -> State -> (State, Lines)
semS (Pen m)        (_, x, y)      = ((m, x, y), [])
semS (MoveTo x2 y2) (Down, x1, y1) = ((Down, x2, y2), [(x1, y1, x2, y2)])
semS (MoveTo x2 y2) (Up, _, _)     = ((Up, x2, y2), [])
semS (Seq c1 c2)     s             = (fst r2, (snd r1) ++ (snd r2))
                                      where r1 = semS c1 s
                                            r2 = semS c2 (fst r1)

--sem'::Cmd3 -> Lines
--
state_init = (Up, 0, 0)

sem'::Cmd3 -> Lines
sem' c = snd (semS c state_init)
