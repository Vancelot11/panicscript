module Panicscript.Sugar where

import Panicscript.Abstract
import Panicscript.Eval

import Data.Map as Map

inc :: Var -> Stmt
inc v = Bind v (Add (Ref v) (Lit 1))

dec :: Var -> Stmt
dec v = Bind v (Sub (Ref v) (Lit 1))

tern :: Test -> Stmt -> Stmt -> Stmt
tern b t f = State St{conds=[b],
                      labels=Map.fromList
                      [(Bits [One],  (t,Break)),
                       (Bits [Zero], (f, Break))]}
