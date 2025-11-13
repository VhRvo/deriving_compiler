{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Arith.Step3D17n where

import Prelude hiding (exp)
import Arith.Def
import Arith.Step1Stacking
import Arith.Step2Cps

eval :: Exp -> [Int]
eval exp = evalD exp [] End

evalD :: Exp -> [Int] -> Cont -> [Int]
-- {- evalD exp stack cont = evalK exp stack (applyCont cont) -}
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
      {- introducing `ContExp` -}
      -- evalK e1 stack (applyCont (ContExp op e2 cont))
      {- unapply `evalD` -}
      evalD e1 stack (ContExp op e2 cont)

data Cont
  = End
  | ContExp Op Exp Cont
  | ContOp Op Cont
  deriving (Eq, Ord, Show)

applyCont :: Cont -> [Int] -> [Int]
applyCont cont =
  case cont of
    End -> id
    ContExp op e2 cont ->
      \s1 ->
        -- evalK e2 s1 (\s2 -> (applyCont cont) (evalOpS op s2))
        {- introducing `ContOp` -}
        -- evalK e2 s1 (applyCont (ContOp op cont))
        {- unapply `evalD` -}
        evalD e2 s1 (ContOp op cont)
    ContOp op cont ->
      \s2 ->
        (applyCont cont) (evalOpS op s2)