module Arith.Step1Stacking where

import Arith.Def

evalS :: Exp -> [Int] -> [Int]
-- evalS exp stack = eval exp : stack
evalS exp stack =
  case exp of
    -- Lit int -> eval (Lit int) : stack
    Lit int -> int : stack
    Bin op e1 e2 ->
    --   eval (Bin op e1 e2) : stack
    --   evalOp op (eval e1) (eval e2) : stack
    {- unapply `evalOpS` -}
      evalOpS op (eval e2 : eval e1 : stack)

evalOpS :: Op -> [Int] -> [Int]
evalOpS op (e2 : e1 : stack) = evalOp op e1 e2 : stack
evalOpS  _ = error "unexpected use of evalOpS"




