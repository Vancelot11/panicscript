module Panicscript where

import Panicscript.Abstract
import Panicscript.TypeCheck
import Panicscript.Eval
import Panicscript.Sugar

import Data.Map as Map
import Prelude hiding (lookup, GT, LT)

-- | Ex1: Loop
-- Sum all of the numbers from 1 to 100.
--     Int sum
--     Int i
--
--     sum = 0
--     n = 1
--     State(n <= 100):
--       1:
--         sum = sum + n
--         n += 1
--         Reval
--
--   >>> runProg ex1
--   Just (fromList [("n",Left 101),("sum",Left 5050)])
--

ex1 :: Prog
ex1 = P [("sum", TInt),("n", TInt)]
   (Block [
       Bind "sum" (Lit 0),
       Bind "n" (Lit 1),
       State St{conds=[LTE (Ref "n") (Lit 100)],
                labels=Map.fromList
                [(Bits [One], (Block [
                   Bind "sum" (Add (Ref "sum") (Ref "n")),
                   Bind "n" (Add (Ref "n") (Lit 1))],
                   Reval))
                ]}
   ])

-- | Ex2: Design code
-- Example implementation of code from Design.md 

ex2 :: Prog
ex2 = P [("x", TInt),("y", TInt)]
   (Block [
       Bind "x" (Lit 1),
       Bind "y" (Lit 3),
       State St{conds=[LTE (Ref "x") (Lit 2), GT (Ref "x") (Ref "y")],
                labels=Map.fromList
                [(Bits [Zero, Zero], (Block [
                   Bind "y" (Sub (Ref "y") (Lit 1))],
                   Reval)),
                 (Bits [Zero, One], (Block [],
                   Go (Bits [One, One]))),
                 (Bits [One, Zero], (Block [
                   Bind "x" (Add (Ref "x") (Lit 1))],
                   Reval)),
                 (Default, (Block [
                   Bind "x" (Ref "y")],
                   None))
                ]}
   ])

-- | Ex3: Function example
--  This example highlights Panicscript's recursive functionality

fib :: Func
fib = F {name="fib",
        args=[(Lit 0)],
        proc=P [("n", TInt), ("return", TInt)]
        (Block [
           State St{conds=[LTE (Ref "n") (Lit 1)],
                    labels=Map.fromList
                    [(Bits [One], ((Bind "return" (Ref "n")), Break)),
                    (Default, (Block [
                      Bind "return" (Add (Exec (name fib) [Sub (Ref "n") (Lit 1)]) (Exec (name fib) [Sub (Ref "n") (Lit 2)]))
                      ]
                      ,Break))]
                   }
        ])
       }

ex3 :: Prog
ex3 = P [("x", TInt), ("fib", TInt)]
   (Block [
    Def "fib" fib,
    Bind "x" (Exec "fib" [Lit 9])
   ])


-- | Ex4
-- This example shows usage of syntactic sugar

ex4 :: Prog
ex4 = P [("x", TInt), ("y", TInt), ("z", TInt), ("w", TInt)]
   (Block [
    Bind "x" (Lit 2),
    Bind "y" (Lit 2),
    inc "x",
    dec "y",
    tern (GT (Ref "x") (Ref "y")) (Bind "z" (Lit 10)) (Bind "z" (Lit 20)),
    tern (LT (Ref "x") (Ref "y")) (Bind "w" (Lit 10)) (Bind "w" (Lit 20))
   ])

-- |  Ex5: Type checking
--  This example won't make it past type checking, variable "z" is out of scope

ex5 :: Prog
ex5 = P [("x", TInt), ("f", TInt)]
   (Block [
    Bind "x" (Lit 2),
    Def "f" F{name="f",
              args=[Lit 0],
              proc=P [("x", TInt), ("return", TInt)]
              (Block [
                Bind "return" (Add (Ref "x") (Ref "z"))
              ])
             },
    Bind "x" (Exec "f" [(Ref "x")])
   ])

runExamples = do 
   putStrLn "\nExample 1: Loop"
   putStrLn "\tSums all numbers from 1 to 100\n"
   putStrLn(show (runProg ex1) ++ "\n\n")
   putStrLn "\nExample 2: Design code"
   putStrLn "\tExample implementation of code from Design.md\n"
   putStrLn(show (runProg ex2) ++ "\n\n")
   putStrLn "\nExample 3: Function example"
   putStrLn "\tThis example highlights Panicscript's function definition and recursive functionality\n"
   putStrLn(show (runProg ex3) ++ "\n\n")
   putStrLn "\nExample 4: Sugar example"
   putStrLn "\tThis example shows usage of syntactic sugar\n"
   putStrLn(show (runProg ex4) ++ "\n\n")
   putStrLn "\nExample 5: Type checking"
   putStrLn "\tThis example won't make it past type checking, variable \"z\" is out of scope\n"
   putStrLn(show (runProg ex5) ++ "\n\n")
