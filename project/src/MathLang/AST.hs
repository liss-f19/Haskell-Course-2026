module MathLang.AST
  ( Program(..)
  , Definition(..)
  , Expr(..)
  , Op(..)
  ) where

data Program = Program [Definition] Expr
  deriving (Eq, Show)

data Definition
  = FuncDef String [String] Expr
  | VarDef String Expr
  deriving (Eq, Show)

data Expr
  = Var String
  | ScalarLit Double
  | VecLit [Expr]
  | MatLit [[Expr]]
  | BinOp Op Expr Expr
  | Apply String [Expr]
  | Let String Expr Expr
  | Neg Expr
  deriving (Eq, Show)

data Op = Add | Sub | Mul | Div | Dot
  deriving (Eq, Show)
