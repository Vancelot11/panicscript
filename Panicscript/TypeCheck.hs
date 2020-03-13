module Panicscript.TypeCheck where

import Panicscript.Abstract 

import Data.Map as Map
import Data.Maybe
import Prelude hiding (lookup, GT, LT)

import Debug.Trace

type Env a = Map Var a

-- | Type checking of expressions in environment
typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (Lit _)     _ = Just TInt
typeExpr (Add l r)   m = case (typeExpr l m, typeExpr r m) of
                           (Just TInt, Just TInt) -> Just TInt
                           (Nothing, Just TInt)   -> error("TYPE ERROR: First argument '" ++ show(l) ++ "' to Add undefined or out of scope")
                           (Just TInt, Nothing)   -> error("TYPE ERROR: Second argument '" ++ show(r) ++ "' to Add undefined or out of scope")
                           _                      -> error("TYPE ERROR: expected Int + Int, got: " ++ show(l) ++ " + " ++ show(r))
typeExpr (Sub l r)   m = case (typeExpr l m, typeExpr r m) of
                           (Just TInt, Just TInt) -> Just TInt
                           (Nothing, Just TInt)   -> error("TYPE ERROR: First argument '" ++ show(l) ++ "' to Add undefined or out of scope")
                           (Just TInt, Nothing)   -> error("TYPE ERROR: Second argument '" ++ show(r) ++ "' to Add undefined or out of scope")
                           _                      -> error("TYPE ERROR: expected Int + Int, got: " ++ show(l) ++ " - " ++ show(r))
typeExpr (Ref v)     m = lookup v m
typeExpr (Exec v es) m = lookup v m

-- | Type checking of statements in environment
typeStmt :: Stmt -> Env Type -> Bool
typeStmt (Bind v e) m = case (lookup v m, typeExpr e m) of
                        (Just tv, Just te) -> tv == te
                        _                  -> error("TYPE ERROR: Bind error on: Bind " ++ show(v) ++ " " ++ show(e))
typeStmt (Block ss) m = all (\s -> typeStmt s m) ss
typeStmt (State s)  m = typeState s m
typeStmt (Def v f)  m = case (lookup v m, checkFunc f m) of
                        (Just tv, fb)      -> fb
                        _                  -> error("TYPE ERROR: Function " ++ show(v) ++ " not declared")

-- | Type checks function as program the same way it would execute at runtime
checkFunc :: Func -> Env Type -> Bool
checkFunc (F{name=n, args=as, proc=P ds s}) m = case(lookup n m, typeArgs as ds m) of
                                           (Just tn, ta) -> ta && typeProg (P (ds ++ [(n,(fromJust (lookup n m)))]) s)
                                           _             -> False

-- | Confirm arguments match function definitions
typeArgs :: [Expr] -> [Decl] -> Env Type -> Bool
typeArgs as ds m = case(length as == length ds - 1) of
                         True  -> typeArgs' as ds m
                         False -> error "TYPE ERROR: number of arguments provided don't match function arguments"
  where typeArgs' :: [Expr] -> [Decl] -> Env Type -> Bool
        typeArgs' [] ds m = True
        typeArgs' (a:as) ((l,r):ds) m = case(typeExpr a m) of
                                        (Just te) -> te == r && typeArgs' as ds m
                                        _         -> False

-- | Helper functions for checking State type validity
typeState :: St -> Env Type -> Bool
typeState (St{conds=ts, labels=l}) m = typeLabelStmts (Map.elems l) m

-- | Helper for label statements
typeLabelStmts :: [(Stmt, Action)] -> Env Type -> Bool
typeLabelStmts ((s,a):ls) m = typeStmt s m && typeLabelStmts ls m
typeLabelStmts []         m = True

-- | Type check entire prog
typeProg :: Prog -> Bool
typeProg (P ds s) = typeStmt s (fromList ds)
