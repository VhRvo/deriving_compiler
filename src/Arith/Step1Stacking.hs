{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Arith.Step1Stacking where

import Arith.Def ( Exp(..), Op)
import Arith.Step0Eval ( evalOp, eval, evalK )

eval' :: Exp -> [Int]
eval' expr = evalS expr []

evalS :: Exp -> [Int] -> [Int]
{- evalS expr stack = eval expr : stack -}
evalS expr stack =
  case expr of
    Lit int ->
      -- eval (Lit int) : stack
      {- apply `eval` -}
      -- int : stack
      {- unapply `push` -}
      push int stack
    Bin op e1 e2 ->
      -- eval (Bin op e1 e2) : stack
      {- apply `eval` -}
      -- evalOp op (eval e1) (eval e2) : stack
      {- unapply specification of `evalOpS` -}
      -- evalOpS op (eval e2 : eval e1 : stack)
      {- unapply specification of `evalS` -}
      -- evalOpS op (eval e2 : evalS e1 stack)
      {- unapply specification of `evalS` -}
      evalOpS op (evalS e2 (evalS e1 stack))

push :: Int -> [Int] -> [Int]
push = (:)

{- push (evalOp op v1 v2) stack = evalOpS op (v2 : v1 : stack)  -}
evalOpS :: Op -> [Int] -> [Int]
evalOpS op (v2 : v1 : stack) = push (evalOp op v1 v2) stack
evalOpS _ _ = error "unexpected use of evalOpS"

-- introducing stack to eliminate the capture of variable `v1`
evalSK :: Exp -> ([Int] -> [Int]) -> [Int] -> [Int]
{- evalSK expr cont stack = cont (evalS expr stack) -}
{- evalSK expr cont stack = cont (eval expr : stack) -}
-- evalSK expr cont stack =
{- eta -}
evalSK expr cont =
  case expr of
    Lit int ->
      -- cont (push (eval (Lit int)) stack)
      {- apply `eval` -}
      -- cont (push int stack)
      {- eta -}
      cont . push int
    Bin op e1 e2 ->
      -- cont (push (eval (Bin op e1 e2)) stack)
      {- apply `eval` -}
      -- cont (push (evalOp op (eval e1) (eval e2)) stack)
      {- apply `eval` -}
      -- cont (evalOpS op (eval e2 : eval e1 : stack))
      {- unapply `push` -}
      -- cont (evalOpS op (push (eval e2) (eval e1 : stack)))
      {- unapply specification of `evalSK` -}
      -- evalSK e2 (\s2 -> cont (evalOpS op s2)) (eval e1 : stack)
      {- unapply `push` -}
      -- evalSK e2 (\s2 -> cont (evalOpS op s2)) (push (eval e1) stack)
      {- unapply specification of `evalSK` -}
      -- evalSK e1 (\s1 -> evalSK e2 (\s2 -> cont (evalOpS op s2)) s1) stack
      {- eta -}
      evalSK e1 (evalSK e2 (cont . evalOpS op))

{- applyD4 (evalKSD4 expr cont) stack = evalSK expr (applyD4 cont) stack -}
{-
applyD4 (evalKSD4 expr cont) stack =
  case expr of
    Lit lit ->
      applyD4 (evalKSD4 (Lit lit) cont) stack
      {- apply specification -}
      evalSK (Lit lit) (applyD4 cont) stack
      {- apply `evalSK` -}
      (applyD4 cont . push int) stack
      {- introducing `D4Push` -}
      applyD4 (D4Push int cont) stack
      {- define `Lit` branch of `evalKSD4` -}
      evalKSD4 (Lit lit) cont =
        D4Push int cont
    Bin op e1 e2 ->
      applyD4 (evalKSD4 (Bin op e1 e2) cont) stack
      {- apply specification -}
      evalSK (Bin op e1 e2) (applyD4 cont) stack
      {- apply `evalSK` -}
      evalSK e1 (evalSK e2 (applyD4 cont . evalOpS op)) stack
      {- introducing `D4Op` -}
      evalSK e1 (evalSK e2 (applyD4 (D4Op op cont))) stack
      {- unapply specification -}
      evalSK e1 (applyD4 (evalKSD4 e2 (D4Op op cont))) stack
      {- unapply specification -}
      applyD4 (evalKSD4 e1 (evalKSD4 e2 (D4Op op cont))) stack
      {- define `Bin` branch of `evalKSD4` -}
      evalKSD4 (Bin op e1 e2) cont =
        evalKSD4 e1 (evalKSD4 e2 (D4Op op cont))
-}

data D4
  = D4End
  | D4Push Int D4
  | D4Op Op D4

applyD4 :: D4 -> [Int] -> [Int]
applyD4 cont =
  case cont of
    D4End ->
      id
    D4Push int cont ->
      applyD4 cont . push int
    D4Op op cont ->
      applyD4 cont . evalOpS op

evalKSD4 :: Exp -> D4 -> D4
evalKSD4 expr cont =
  case expr of
    Lit int ->
      D4Push int cont
    Bin op e1 e2 ->
      evalKSD4 e1 (evalKSD4 e2 (D4Op op cont))
