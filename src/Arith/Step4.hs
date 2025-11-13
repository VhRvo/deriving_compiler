{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Arith.Step4 where

import Prelude hiding (exp)
import Arith.Def
import Arith.Step1Stacking
import Arith.Step2Cps

data Instr
  = EvalLeft Op Exp
  | EvalRight Op
  deriving (Eq, Ord, Show)

eval :: Exp -> [Int]
eval exp = evalI exp [] []


evalI :: Exp -> [Int] -> [Instr] -> [Int]
evalI exp stack cont = evalD
evalD exp stack cont =
  case exp of
    Lit int ->
      -- evalK (Lit int) stack (applyCont cont)
      {- apply `evalK` -}
      applyCont cont (int : stack)
    Bin op e1 e2 ->
      -- evalK (Bin op e1 e2) stack (applyCont cont)
      {- apply `evalK` -}
      -- evalK e1 stack (\s1 -> evalK e2 s1 (\s2 -> (applyCont cont) (evalOpS op s2)))
      {- introducing `EvalLeft` -}
      -- evalK e1 stack (applyCont (EvalLeft op e2 cont))
      {- unapply `evalD` -}
      evalD e1 stack (EvalLeft op e2 cont)

data Cont
  deriving (Eq, Ord, Show)

applyCont :: Cont -> [Int] -> [Int]
applyCont cont =
  case cont of
    End -> id
    EvalLeft op e2 cont ->
      \s1 ->
        -- evalK e2 s1 (\s2 -> (applyCont cont) (evalOpS op s2))
        {- introducing `EvalRight` -}
        -- evalK e2 s1 (applyCont (EvalRight op cont))
        {- unapply `evalD` -}
        evalD e2 s1 (EvalRight op cont)
    EvalRight op cont ->
      \s2 ->
        (applyCont cont) (evalOpS op s2)