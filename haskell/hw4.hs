-------------------------------------------------------------------------
-- Group members：Qiulin Zhang, Zhuoling Chen, Sangyeon Lee, Ingyu Woo  
-- CS 381, Spring 2018                                                  
-- Homework 4 (Runtime Stack, Scoping, Parameter Passing)                                        
-- Due：May 24, 2018，2pm                                             
--------------------------------------------------------------------------

{-
    Exercise 1. Runtime Stack

    {
        int x;
        int y;
        y:=1; 
        { int f (int x) {
            if x=0 then {
                y := 1 }
            else  {
                y := f(x-1)*y+1 };
            return y;
          };
          x := f(2);
        };
    }
-}

  []
1 [x:?]
2 [y:?, x:?]
3 [y:1, x:?]
4 [f:{}, y:1, x:?]
11 >>
    [x:2, f:{}, y:1, x:?]
  5 [x:2, f:{}, y:1, x:?]
  7 [x:2, f:{}, y:1, x:?]
  8 >>
        [x:1, x:2, f:{}, y:1, x:?]
      5 [x:1, x:2, f:{}, y:1, x:?]
      7 [x:1, x:2, f:{}, y:1, x:?]
      8 >>
            [x:0, x:1, x:2, f:{}, y:1, x:?]
          5 [x:0, x:1, x:2, f:{}, y:1, x:?]
          6 [x:0, x:1, x:2, f:{}, y:1, x:?]
          9 [res:1, x:0, x:1, x:2, f:{}, y:1, x:?]
        <<
      8 [x:1, x:2, f:{}, y:2, x:?]
      9 [res:2, x:1, x:2, f:{}, y:2, x:?]
    <<
  8 [x:2, f:{}, y:5, x:?]
  9 [res:5, x:2, f:{}, y:5, x:?]
<<
11 [f:{}, y:5, x:5]
12 [y:5, x:5]
13 []



{-
    Exercise 2. Static and Dynamic Scope

    { int x;
      int y;
      int z;
      x := 3;
      y := 7;
      { int f(int y) { return x*y };
        int y;
        y := 11;
        { int g(int x) { return f(y) };
         { int y;
           y := 13;
           z := g(2);
         };
        };
      };
    }
-}



-- (a) Which value will be assigned to z in line 12 under static scoping?
--    : z=33
--   (The below is the runtime stack for this problem)
--

   []
1  [x:?]
2  [y:?, x:?]
3  [z:?, y:?, x:?]
4  [z:?, y:?, x:3]
5  [z:?, y:7, x:3]
6  [f:{}, z:?, y:7, x:3]
7  [y:?, f:{}, z:?, y:7, x:3]
8  [y:11, f:{}, z:?, y:7, x:3]
9  [g:{}, y:11, f:{}, z:?, y:7, x:3]
10 [y:?, g:{}, y:11, f:{}, z:?, y:7, x:3]
11 [y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
12 >>
     [x:2, y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
   9 >>
         [y:11, x:2, y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
       6 [res:33, y:11, x:2, y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
     <<
   9 [res:33, x:2, y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
   <<
12 [y:13, g:{}, y:11, f:{}, z:33, y:7, x:3]


-- (b) Which value will be assigned to z in line 12 under dynamic scoping?
--    : z=26
--   (The below is the runtime stack for this problem)
--

   []
1  [x:?]
2  [y:?, x:?]
3  [z:?, y:?, x:?]
4  [z:?, y:?, x:3]
5  [z:?, y:7, x:3]
6  [f:{}, z:?, y:7, x:3]
7  [y:?, f:{}, z:?, y:7, x:3]
8  [y:11, f:{}, z:?, y:7, x:3]
9  [g:{}, y:11, f:{}, z:?, y:7, x:3]
10 [y:?, g:{}, y:11, f:{}, z:?, y:7, x:3]
11 [y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
12 >>
     [x:2, y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
   9 >>
         [y:13, x:2, y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
       6 [res:26, y:13, x:2, y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
     <<
   9 [res:26, x:2, y:13, g:{}, y:11, f:{}, z:?, y:7, x:3]
   <<
12 [y:13, g:{}, y:11, f:{}, z:26, y:7, x:3]

{-
    Exercise 3. Parameter Passing

    { int y;
      int z;
      y := 7;
      { int f(int a) {
          y := a+1;
          return (y+a)
        };
        int g(int x) {
          y := f(x+1)+1;
          z := f(x-y+3);
          return (z+1)
        };
        z := g(y*2);
      };
    }

-}

-- (a) Call-by-Name 
--    : y=54, z=112 
--   (The below is the runtime stack for this problem)

   []
1  [y:?]
2  [z:?, y:?]
3  [z:?, y:7]
4  [f:{}, z:?, y:7]
8  [g:{}, f:{}, z:?, y:7]
13 >>
        [x:y*2,g:{},f:{}, z:?, y:7]
     9  >>
            [a:x+1, x:y*2, g:{}, f:{}, z:?, y:7]
          5 [a:x+1, x:y*2, g:{}, f:{}, z:?, y:16]
          6 [res:49, a:x+1, x:y*2, g:{}, f:{}, z:?, y:16]
        <<
     9  [x:y*2, g:{}, f:{}, z:?, y:50]
     10 >>
            [a:x-y+3, x:y*2, g:{}, f:{}, z:?, y:50]
          5 [a:x-y+3, x:y*2, g:{}, f:{}, z:?, y:54]
          6 [res:111, a:x-y+3, x:y*2, g:{}, f:{}, z:?, y:54]
        <<
     10 [x:y*2, g:{}, f:{}, z:111, y:54]
     11 [res:112, x:y*2, g:{}, f:{}, z:111, y:54]
   <<
13 [g:{}, f:{}, z:112, y:54]
14 [z:112, y:54]
15 []


-- (b) Call-by Need
--    : y=-14, z=-28 
--   (The below is the runtime stack for this problem)
--
  []
1 [y:?]
2 [z:?, y:?]
3 [z:?, y:7]
4 [f:{}, z:?, y:7]
8 [g:{}, f:{}, z:?, y:7]
13 >>
       [x:y*2, g:{}, f:{}, z:? y:7]
     9 >>
           [a:x+1, x:y*2, g:{}, f:{}, z:? y:7]
         5 [a:15, x:14, g:{}, f:{}, z:? y:16]
         6 [res:31, a:15, x:14, g:{}, f:{}, z:?, y:16]
       <<
     9 [x:14, g:{}, f:{}, z:?, y:32]
    10 >>
           [a:x-y+3, x:14, g:{}; f:{}, z:?, y:32]
         5 [a:-15, x:14, g:{}, f:{}, z:? y:-14]
         6 [res:-29, a:-15, x:14, g:{}, f:{}, z:?, y:-14]
       <<
    10 [x:14, g:{}, f:{}, z:-29, y:-14]
    11 [res:-28, x:14, g:{}, f:{}, z:-29, y:-14]
   <<
13 [g:{}, f:{}, z:-28, y:-14]
14 [z:-28, y:-14]
15 []

