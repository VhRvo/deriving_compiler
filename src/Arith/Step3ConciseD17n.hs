{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Arith.Step3ConciseD17n where

import Arith.Def ( Exp(..), Op )
import Arith.Step1Stacking

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
      {- introducing `ContOp` -}
      -- evalK e1 stack (\s1 -> evalK e2 s1 (applyCont (ContOp op cont)))
      {- unapply specification of `evalD` -}
      -- evalK e1 stack (\s1 -> evalD e2 s1 (ContOp op cont))
      {- introducing local binding -}
      -- let cont' = ContOp op cont
      --  in evalK e1 stack (\s1 -> evalD e2 s1 cont')
      {- introducing `ContExp` -}
      -- let cont' = ContOp op cont
      --  in evalK e1 stack (applyCont (ContExp e2 cont'))
      {- unapply specification of `evalD` -}
      -- let cont' = ContOp op cont
      --  in evalD e1 stack (ContExp e2 cont')
      {- inlining local binding -}
      evalD e1 stack (ContExp e2 (ContOp op cont))

data Cont
  = ContEnd
  | ContOp Op Cont
  | ContExp Exp Cont
  deriving (Eq, Ord, Show)

applyCont :: Cont -> [Int] -> [Int]
applyCont cont =
  case cont of
    ContEnd -> id
    ContOp op cont ->
      \s2 ->
        (applyCont cont) (evalOpS op s2)
    ContExp e2 cont' ->
      -- assert cont'@(ContOp op cont)
      \s1 -> evalD e2 s1 cont'
