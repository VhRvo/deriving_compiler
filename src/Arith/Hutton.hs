{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Arith.Hutton where

import Arith.Def
import Arith.Step1Stacking hiding (evalSK)

evalSK :: Exp -> ([Int] -> [Int]) -> [Int] -> [Int]
{- evalSK expr cont stack = cont (evalS expr stack) -}
-- evalSK expr cont stack =
{- apply eta -}
evalSK expr cont =
  case expr of
    Lit int ->
      -- cont (evalS (Lit int) stack)
      {- apply `evalS` -}
      -- cont (push int stack)
      {- apply eta -}
      cont . push int
    Bin op e1 e2 ->
      -- cont (evalS (Bin op e1 e2) stack)
      {- apply `evalS` -}

      {- path 1 -}
      -- cont (evalOpS op (evalS e2 (( evalS e1 stack )) ))
      {- unapply specification of `evalSK` -}
      -- evalSK e1 (\s1 -> cont (evalOpS op ((evalS e2 s1)) )) stack
      {- unapply specification of `evalSK` -}
      -- evalSK e1 (\s1 -> evalSK e2 (\s2 -> cont (evalOpS op s2)) s1) stack

      {- path 2 -}
      -- cont (evalOpS op (( evalS e2 (evalS e1 stack) )) )
      {- unapply specification of `evalSK` -}
      -- evalSK e2 (\s2 -> cont (evalOpS op s2)) (( evalS e1 stack ))
      {- unapply specification of `evalSK` -}
      -- evalSK e1 (\s1 -> evalSK e2 (\s2 -> cont (evalOpS op s2)) s1) stack
      {- apply eta -}
      evalSK e1 (evalSK e2 (cont . evalOpS op))


{-
applyD (evalSKD expr cont) = evalSK expr (applyD cont)

case expr of
  Lit lit:
    applyD (evalSKD (Lit lit) cont) =
      evalSK (Lit lit) (applyD cont)
      {- apply `evalSK` -}
      applyD cont . push int
      {- introducing `Push` -}
      applyD (Push int cont)
  Bin op e1 e2:
    applyD (evalSKD (Bin op e1 e2) cont) =
      evalSK (Bin op e1 e2) (applyD cont)
      {- apply `evalSK` -}
      evalSK e1 (evalSK e2 (applyD cont . evalOpS op))
      {- introducing `Op` -}
      evalSK e1 (evalSK e2 (applyD (Op op cont)))
-}
