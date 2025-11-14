{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
module Arith.Step2Cps where

import Arith.Def ( Exp(..) )
import Arith.Step1Stacking
import Prelude hiding (exp)

eval :: Exp -> [Int]
eval exp = evalK exp [] id

evalK :: Exp -> [Int] -> ([Int] -> [Int]) -> [Int]
{- evalK exp stack cont = cont (evalS exp stack) -}
evalK exp stack cont =
  case exp of
    Lit int ->
      -- cont (evalS (Lit int) stack)
      {- apply `evalS` -}
      cont (push int stack)
    Bin op e1 e2 ->
      -- cont (evalS (Bin op e1 e2) stack)
      {- apply `evalS` -}

      {- path 1 -}
      -- cont (evalOpS op (evalS e2 (evalS e1 stack)))
      {- unapply `evalK` -}
      -- evalK e1 stack (\s1 -> cont (evalOpS op (evalS e2 s1)))
      {- unapply `evalK` -}
      -- evalK e1 stack (\s1 -> evalK e2 s1 (\s2 -> cont (evalOpS op s2)))

      {- path 2 -}
      -- cont (evalOpS op (evalS e2 (evalS e1 stack)))
      {- unapply `evalK` -}
      -- evalK e2 (evalS e1 stack) (\s2 -> cont (evalOpS op s2))
      {- unapply `evalK` -}
      evalK e1 stack (\s1 -> evalK e2 s1 (\s2 -> cont (evalOpS op s2)))
