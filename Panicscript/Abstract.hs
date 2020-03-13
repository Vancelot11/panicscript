module Panicscript.Abstract where

import Data.Map as Map

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
type Var  = String

data Expr = Lit Int        -- literal integer
          | Add Expr Expr  -- integer addition
          | Sub Expr Expr  -- integer subtraction
          | Ref Var        -- variable reference
          | Exec Var [Expr]
  deriving (Eq,Show)

data Test = LTE Expr Expr  -- less than or equal to
          | LT  Expr Expr  -- less than
          | GTE Expr Expr  -- greater than or equal to
          | GT  Expr Expr  -- greater than
          | Not Expr       -- boolean negation
  deriving(Eq, Show)

data Stmt = Bind Var Expr  -- assign expression to variable
          | Block [Stmt]   -- block of code
          | State St       -- state structure
          | Def Var Func   -- define function and assign to function name
  deriving (Eq,Show)

-- | Bitstr data structure for state structure
data Bitstr = Zero | One deriving(Eq,Show,Ord)
-- | Label structure
data Label = Bits [Bitstr] | Default deriving(Eq,Show, Ord)
-- | Actions in label
data Action = Reval | Break | Go Label | None deriving(Eq,Show)

-- | Map labels for lookup
type LMap = Map.Map Label (Stmt, Action)

-- | State control structure
data St = St { conds  :: [Test]
             , labels :: LMap
             } deriving(Eq, Show)

-- | Define types
data Type = TInt | TBool
  deriving (Eq,Show)

-- | Variable and function declarations
type Decl = (Var, Type)

-- | Function Structure
data Func = F { name :: Var
              , args :: [Expr]
              , proc :: Prog
} deriving(Eq, Show)

-- | Program structure
data Prog = P [Decl] Stmt
  deriving (Eq,Show)
