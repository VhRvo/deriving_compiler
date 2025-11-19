{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Arith.Step2Cps where

import Arith.Def ( Exp(..) )
import Arith.Step1Stacking ( evalOpS, push )

eval :: Exp -> [Int]
eval expr = evalSK expr [] id

evalSK :: Exp -> [Int] -> ([Int] -> [Int]) -> [Int]
{- evalSK expr stack cont = cont (evalS expr stack) -}
evalSK expr stack cont =
  case expr of
    Lit int ->
      -- cont (evalS (Lit int) stack)
      {- apply `evalS` -}
      cont (push int stack)
    Bin op e1 e2 ->
      -- cont (evalS (Bin op e1 e2) stack)
      {- apply `evalS` -}

      {- path 1 -}
      -- cont (evalOpS op (evalS e2 (evalS e1 stack)))
      {- unapply specification of `evalSK` -}
      -- evalSK e1 stack (\s1 -> cont (evalOpS op (evalS e2 s1)))
      {- unapply specification of `evalSK` -}
      -- evalSK e1 stack (\s1 -> evalSK e2 s1 (\s2 -> cont (evalOpS op s2)))

      {- path 2 -}
      -- cont (evalOpS op (evalS e2 (evalS e1 stack)))
      {- unapply specification of `evalSK` -}
      -- evalSK e2 (evalS e1 stack) (\s2 -> cont (evalOpS op s2))
      {- unapply specification of `evalSK` -}
      evalSK e1 stack (\s1 -> evalSK e2 s1 (\s2 -> cont (evalOpS op s2)))
