{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Arith.Step3D17n where

import Arith.Def ( Exp(..), Op )
import Arith.Step1Stacking ( evalOpS, push )

eval :: Exp -> [Int]
eval expr = evalD expr [] ContEnd

evalD :: Exp -> [Int] -> Cont -> [Int]
-- {- evalD expr stack cont = evalK expr stack (applyCont cont) -}
evalD expr stack cont =
  case expr of
    Lit int ->
      -- evalK (Lit int) stack (applyCont cont)
      {- apply `evalK` -}
      applyCont cont (push int stack)
    Bin op e1 e2 ->
      -- evalK (Bin op e1 e2) stack (applyCont cont)
      {- apply `evalK` -}
      -- evalK e1 stack (\s1 -> evalK e2 s1 (\s2 -> (applyCont cont) (evalOpS op s2)))
      {- introducing `ContExp` -}
      -- evalK e1 stack (applyCont (ContExp op e2 cont))
      {- unapply specification of `evalD` -}
      evalD e1 stack (ContExp op e2 cont)

data Cont
  = ContEnd
  | ContExp Op Exp Cont
  | ContOp Op Cont
  deriving (Eq, Ord, Show)

applyCont :: Cont -> [Int] -> [Int]
applyCont cont =
  case cont of
    ContEnd -> id
    ContExp op e2 cont ->
      \s1 ->
        -- evalK e2 s1 (\s2 -> (applyCont cont) (evalOpS op s2))
        {- introducing `ContOp` -}
        -- evalK e2 s1 (applyCont (ContOp op cont))
        {- unapply specification of `evalD` -}
        evalD e2 s1 (ContOp op cont)
    ContOp op cont ->
      \s2 ->
        (applyCont cont) (evalOpS op s2)
