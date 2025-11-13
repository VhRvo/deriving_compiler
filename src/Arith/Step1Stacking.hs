module Arith.Step1Stacking where

import Prelude hiding (exp)
import Arith.Def

eval :: Exp -> [Int]
eval exp = evalS exp []

evalS :: Exp -> [Int] -> [Int]
{- evalS exp stack = eval exp : stack -}
evalS exp stack =
  case exp of
    Lit int ->
      -- eval (Lit int) : stack
      {- apply `eval` -}
      int : stack
    Bin op e1 e2 ->
      -- eval (Bin op e1 e2) : stack
      {- apply `eval` -}
      -- evalOp op (eval e1) (eval e2) : stack
      {- unapply `evalOpS` -}
      --   evalOpS op (eval e2 : eval e1 : stack)
      {- unapply `evalS` -}
      --   evalOpS op (eval e2 : evalS e1 stack)
      {- unapply `evalS` -}
      evalOpS op (evalS e2 (evalS e1 stack))

evalOpS :: Op -> [Int] -> [Int]
evalOpS op (e2 : e1 : stack) = evalOp op e1 e2 : stack
evalOpS _ _ = error "unexpected use of evalOpS"
