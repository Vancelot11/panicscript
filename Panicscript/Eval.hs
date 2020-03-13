module Panicscript.Eval where

import Data.Map as Map
import Data.Maybe
import qualified Data.List as List
import Prelude hiding (lookup, GT, LT)

import Panicscript.Abstract
import Panicscript.TypeCheck

import Debug.Trace

data Val = Num Int
         | Truth Bool
         | Function Func
  deriving(Eq, Show)

-- | Evaluate expressions
evalExpr :: Expr -> Env Val -> Val
evalExpr (Lit i)     _ = Num i
evalExpr (Add l r)   m = Num (evalInt l m + evalInt r m)
evalExpr (Sub l r)   m = Num (evalInt l m - evalInt r m)
evalExpr (Ref x)     m = case lookup x m of
                           Just v  -> v
                           Nothing -> error "internal error: undefined variable"
evalExpr (Exec f es) m = case lookup f m of
                          Just (Function v) -> evalFunc v' m
                             where
                               v' = F{name=name v, args=es, proc=proc v}
                          Nothing -> error "internal error: undefined function"

-- | Eval Functions as separate program with arguments
evalFunc :: Func -> Env Val -> Val
evalFunc (F{name=n, args=as, proc=P ds s}) m = case runProg (P d' s') of
                                             Just v -> fromJust (lookup "return" v)
  where
    d' = ds ++ [(n, (fromJust (List.lookup "return" ds)))]
    s' = Block ([Def n F{name=n, args=as, proc=P ds s}] ++ bindArgs as ds m ++ [s])

-- | Add code that positionally assigns arguments to function parameters
bindArgs :: [Expr] -> [Decl] -> Env Val -> [Stmt]
bindArgs (a:as) ((l,r):ds) m = [Bind l (Lit (evalInt a m))]

-- | Evalate boolean tests
evalTest :: Test -> Env Val -> Val
evalTest (LTE l r) m = Truth (evalInt l m <= evalInt r m)
evalTest (LT  l r) m = Truth (evalInt l m <  evalInt r m)
evalTest (GTE l r) m = Truth (evalInt l m >= evalInt r m)
evalTest (GT  l r) m = Truth (evalInt l m >  evalInt r m)
evalTest (Not e)   m = Truth (not (evalBool e m))

-- | Evaluate Integer expressions
evalInt :: Expr -> Env Val -> Int
evalInt e m = case evalExpr e m of
                Num i  -> i
                Truth _ -> error "internal error: expected Int got Bool"

-- | Evaluate Booleans
evalBool :: Expr -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 Truth b -> b
                 Num _  -> error "internal error: expected Bool got Int"

-- | Evaluate Statement
evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (Bind x e) m = insert x (evalExpr e m) m
evalStmt (Block ss) m = evalStmts ss m
evalStmt (State s)  m = evalState s m
evalStmt (Def v f)  m = insert v (Function f) m

-- | Evaluate Blocks
evalStmts :: [Stmt] -> Env Val -> Env Val
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)

-- | Evaluate State control/loop structure
evalState :: St -> Env Val -> Env Val
evalState s m = case Map.lookup (Bits (getState (conds s) m)) (labels s) of
                                         (Just (b,a)) -> execState s (b,a) m
                                         (Nothing) -> case Map.lookup (Default) (labels s) of
                                                      (Just (bl,ac)) -> execState s (bl,ac) m
                                                      (Nothing) -> m

-- | Execute label in State structure
execState :: St -> (Stmt, Action) -> Env Val -> Env Val
execState s (b, a) m = case a of
                       (Reval) -> (evalState s) (evalStmt b m)
                       (Break) -> evalStmt b m
                       (Go l)  -> case Map.lookup (l) (labels s) of
                                  (Just (bl, ac)) -> execState s (bl,ac) (evalStmt b m)
                                  (Nothing) -> m

-- | Evaluate conditions to determine bitstring
getState :: [Test] -> Env Val -> [Bitstr]
getState (t:ts) m = case evalTest t m of
                    (Truth True) -> [One] ++ getState ts m
                    (Truth False) -> [Zero] ++ getState ts m
getState []     m = []

-- | Evaluate Program
evalProg :: Prog -> Env Val
evalProg (P ds s) = evalStmt s m
  where
    m = fromList (Prelude.map (\(x,t) -> (x, init t)) ds)
    init TInt  = Num 0
    init TBool = Truth False

-- | TypeCheck and run program
runProg :: Prog -> Maybe (Env Val)
runProg p = if typeProg p then Just (evalProg p)

                          else Nothing
