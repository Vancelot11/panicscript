module Panicscript where

import Data.Map as Map
import Data.Maybe (fromJust)
import Prelude hiding (lookup, GT, LT)

import Debug.Trace
import Data.Typeable

-- | Abstract syntax of expressions.
--
--     expr  ::=  int
--            |   expr + expr
--            |   expr ≤ expr
--            |   expr ≤= expr
--            |   expr > expr
--            |   expr >= expr
--            |   expr == expr
--            |   `!` expr
--            |   var

-- | Variables.
type Var = String

data Expr = Lit Int        -- literal integer
          | Add Expr Expr  -- integer addition
          | Sub Expr Expr  -- integer subtraction
          | Ref Var        -- variable reference
  deriving (Eq,Show)

data Test = LTE Expr Expr  -- less than or equal to
          | LT  Expr Expr  -- less than
          | GTE Expr Expr  -- greater than or equal to
          | GT  Expr Expr  -- greater than
          | Not Expr       -- boolean negation
  deriving(Eq, Show)

data Stmt = Bind Var Expr
          | Block [Stmt]
          | State St
  deriving (Eq,Show)

data Bitstr = Zero | One deriving(Eq,Show,Ord)
data Label = Bits [Bitstr] | Default deriving(Eq,Show, Ord)
data Action = Reval | Break | Go Label | None deriving(Eq,Show)

type LMap = Map.Map Label (Stmt, Action)

data St = St { conds  :: [Test]
             , labels :: LMap
             } deriving(Eq, Show)

data Type = TInt | TBool
  deriving (Eq,Show)

type Decl = (Var, Type)

data Prog = P [Decl] Stmt
  deriving (Eq,Show)

-- | Example program: sum all of the numbers from 1 to 100.
--
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

testState :: St
testState = St{conds=[LTE (Ref "n") (Lit 100), LTE (Ref "n") (Lit 99), LTE (Ref "n") (Lit 0)],
                labels=Map.fromList
                [(Bits [One], (Block [
                   Bind "sum" (Add (Ref "sum") (Ref "n")),
                   Bind "n" (Add (Ref "n") (Lit 1))],
                   Reval)),
                 (Bits [Zero], (Block [
                   Bind "sum" (Add (Ref "sum") (Ref "n")),
                   Bind "n" (Add (Ref "n") (Lit 1))],
                   Reval)),
                 (Default, (Block [
                   Bind "sum" (Add (Ref "sum") (Ref "n")),
                   Bind "n" (Add (Ref "n") (Lit 1))],
                   Reval))
                ]}
testDs :: [Decl]
testDs = [("sum", TInt), ("n", TInt)]


type Env a = Map Var a

-- | Type checking of expressions in environment
typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (Lit _)   _ = Just TInt
typeExpr (Add l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TInt
                         _                      -> Nothing
typeExpr (Sub l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TInt
                         _                      -> Nothing
typeExpr (Ref v)   m = lookup v m

-- | Type checking of statements in environment
typeStmt :: Stmt -> Env Type -> Bool
typeStmt (Bind v e) m = case (lookup v m, typeExpr e m) of
                        (Just tv, Just te) -> tv == te
                        _                  -> False
typeStmt (Block ss) m = all (\s -> typeStmt s m) ss
typeStmt (State s)  m = typeState s m

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


type Val = Either Int Bool

evalExpr :: Expr -> Env Val -> Val
evalExpr (Lit i)   _ = Left i
evalExpr (Add l r) m = Left (evalInt l m + evalInt r m)
evalExpr (Sub l r) m = Left (evalInt l m - evalInt r m)
evalExpr (Ref x)   m = case lookup x m of
                         Just v  -> v
                         Nothing -> error "internal error: undefined variable"

evalTest :: Test -> Env Val -> Val
evalTest (LTE l r) m = Right (evalInt l m <= evalInt r m)
evalTest (LT  l r) m = Right (evalInt l m <  evalInt r m)
evalTest (GTE l r) m = Right (evalInt l m >= evalInt r m)
evalTest (GT  l r) m = Right (evalInt l m >  evalInt r m)
evalTest (Not e)   m = Right (not (evalBool e m))

evalInt :: Expr -> Env Val -> Int
evalInt e m = case evalExpr e m of
                Left i  -> i
                Right _ -> error "internal error: expected Int got Bool"

evalBool :: Expr -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 Right b -> b
                 Left _  -> error "internal error: expected Bool got Int"

evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (Bind x e) m = insert x (evalExpr e m) m
evalStmt (Block ss) m = evalStmts ss m
evalStmt (State s)  m = evalState s m

evalStmts :: [Stmt] -> Env Val -> Env Val
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)

evalState :: St -> Env Val -> Env Val
evalState s m = case Map.lookup (Bits (getState (conds s) m)) (labels s) of
                                         (Just (b,a)) -> execState s (b,a) m
                                         (Nothing) -> case Map.lookup (Default) (labels s) of
                                                      (Just (bl,ac)) -> execState s (bl,ac) m
                                                      (Nothing) -> m

execState :: St -> (Stmt, Action) -> Env Val -> Env Val
execState s (b, a) m = case trace("\n>>> m: " ++ show m ++ "b: " ++ show b ++ show a)a of
                       (Reval) -> (evalState s) (evalStmt b m)
                       (Break) -> evalStmt b m
                       (Go l)  -> case Map.lookup (l) (labels s) of
                                  (Just (bl, ac)) -> execState s (bl,ac) (evalStmt b m)
                                  (Nothing) -> m

getState :: [Test] -> Env Val -> [Bitstr]
getState (t:ts) m = case evalTest t m of
                    (Right True) -> [One] ++ getState ts m
                    (Right False) -> [Zero] ++ getState ts m
getState [] m = []

evalProg :: Prog -> Env Val
evalProg (P ds s) = evalStmt s m
  where
    m = fromList (Prelude.map (\(x,t) -> (x, init t)) ds)
    init TInt  = Left 0
    init TBool = Right False

runProg :: Prog -> Maybe (Env Val)
runProg p = if typeProg p then Just (evalProg p)

                          else Nothing
