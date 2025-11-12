module Arith.Def where

data Op
  = Add
  | Sub
  | Mul
  deriving (Eq, Ord, Show)

data Exp
  = Lit Int
  | Bin Op Exp Exp

eval :: Exp -> Int
eval =
  \case
    Lit int -> int
    Bin op e1 e2 -> evalOp op (eval e1) (eval e2)

evalOp :: Op -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)

