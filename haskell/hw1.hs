-------------------------------------------------------------------------
 
-- CS 381, Spring 2018                                                  
-- Homework 1 (Abstract Syntax)                                        
-- Due：April 26, 2018，2pm                                             
--------------------------------------------------------------------------

module HW1 where

{-
    Exercise 1. Mini Logo

    cmd  ::= pen mode
           | moveto (pos,pos)
           | def name ( pars ) cmd
           | call name ( vals )
           | cmd; cmd
    mode ::= up | down
    pos  ::= num | name
    pars ::= name, pars | name
    vals ::= num, vals | num
-}

-- 1. (a)
--
data Cmd = Pen Mode
         | MoveTo Pos Pos
         | Def String Pars Cmd
         | Call String Vals
         | AddCmd Cmd Cmd
         deriving Show

data Mode = Up
          | Down
          deriving Show

data Pos = PosNum Int
         | PosName String
         deriving Show

data Pars = Pars String Pars
          | Par String
          deriving Show

data Vals = Vals Int Vals
          | Val Int
          deriving Show

-- 1. (b)
--
vector = Def
         ("vector")
         (Pars "x1" (Pars "y1" (Pars "x2" (Par "y2"))))
         (AddCmd (Pen Up)
         (AddCmd (MoveTo (PosName "x1") (PosName"y1"))
         (AddCmd (Pen Down)
         (AddCmd (MoveTo (PosName "x2") (PosName "y2"))
         (Pen Up)))))

-- 1. (c)
--
steps :: Int -> Cmd
steps 0 = Pen Up
steps n = AddCmd (Pen Up)
          (AddCmd (MoveTo (PosNum n) (PosNum n))
          (AddCmd (Pen Down)
          (AddCmd (MoveTo (PosNum (n-1)) (PosNum n))
          (AddCmd (MoveTo (PosNum (n-1)) (PosNum (n-1))) (steps (n-1))))))



{-
    Exercise 2. Digital Circuit Design Language

    circuit ::= gates links
    gates   ::= num:gateFn ; gates | ε
    gateFn  ::= and |or |xor |not
    links   ::= from num.num to num.num; links | ε
-}

-- 2. (a)
--
data Circuit = C (Gates, Links)
               deriving Show

type Gates  = [Gate]
type Gate   = (Int, GateFn)
data GateFn = And | Or | Xor | Not
              deriving Show

type Links = [Link]
type Link  = (Int, Int, Int, Int)

-- 2. (b)
--
halfadder = C ([(1, Xor), (2, And)], [(1,1,2,1), (1,2,2,2)])

-- 2. (c)
--
-- the main printing fuction that utilizes the two helper functions below
prettyPrt :: Circuit -> IO ()
prettyPrt (C (g, l)) = putStr((gatePrt g) ++ (linkPrt l)) 

-- a helper function that outputs the content of "data Gates" in the format of "String"
gatePrt :: Gates -> String
gatePrt [] = ""
gatePrt ((i, g):xr) = (show i) ++ ":" ++ (show g) ++ ";\n" ++ (gatePrt xr)

-- a helper function that outputs the content of "data Links" in the format of "String"
linkPrt :: Links -> String
linkPrt [] = ""
linkPrt ((f1,f2,t1,t2):xr) = "from " ++ (show f1)
                             ++ "." ++ (show f2)
                             ++ " to " ++ (show t1)
                             ++ "." ++ (show t2)
                             ++ "\n" ++ (linkPrt xr)



{-
    Exercise 3. Designing Abstract Syntax
-}

-- The 1st representation
data Expr = N Int
          | Plus Expr Expr
          | Times Expr Expr
          | Neg Expr
          deriving Show

-- The 2nd representation
data Op  = Add | Multiply | Negate
         deriving Show
data Exp = Num Int
         | Apply Op [Exp]
         deriving Show

-- 3. (a)
--
expr3a = Apply Negate [(Apply Multiply [(Apply Add [Num 3, Num 4]), (Num 7)])]

-- 3. (b)
--
-- The 1st representation is simple and easy to understand since each constructor
-- intuitively indicates both the type of mathematical operations that it behaves as
-- and the number of the operands required for each operation. For example, the constructor
-- Plus requires two arguments as operands while the constructor Neg requires only one,
-- which makes sense in a mathematical perspective. However, the usage of this abstract syntax
-- may seem complex and redundant especially when a constructor is consecutively applied.
-- For example, a mathematical expression "1 + 2 + 3" requires the two times of a constructor
-- Plus in row, which produces “(Plus (Plus 1 2) (3))”.

-- On the other hand, the 2nd representation solves this problem of the complexity and
-- the redundancy by utilizing a list for the operands of a mathematical operation.
-- For example, "1 + 2 + 3" is expressed in this abstract syntax “Apply Add [ Num 1, Num 2, Num3 ]”.
-- However, this utilization of a list may possess the advantage of the previous abstract syntax
-- as a disadvantage. The problem is that the incorrect number of the operands for a mathematical
-- operation may be accepted as a legal operation whereas the previous abstract syntax strictly
-- restrict the number of the operands for each mathematical operation. For example,
-- as a legal operation in the 2nd abstract syntax, “Apply Add [ Num 1 ]” will be accepted 
-- even though operation Add actually needs two operands for the legal operation.
-- However, this abstract syntax shows more organized semantic domain by separating data types
-- in accordance with their functionality. For example, since all mathematical operations are
-- in one data type “data Op”, all mathematical expressions, which requires the operations,
-- can be described by “Apply Op [Exp]”.

-- 3. (c)
--
translate :: Expr -> Exp
translate (N i)        = Num i
translate (Plus e e')  = Apply Add [translate e, translate e']
translate (Times e e') = Apply Multiply [translate e, translate e']
translate (Neg e)      = Apply Negate [translate e]

