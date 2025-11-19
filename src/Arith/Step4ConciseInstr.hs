{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

module Arith.Step4ConciseInstr where

import Arith.Def
import Arith.Step1Stacking

data ContAction
  = Op Op
  | Exp Exp
  deriving (Eq, Ord, Show)

eval :: Exp -> [Int]
eval expr = evalD expr [] []

evalD :: Exp -> [Int] -> [ContAction] -> [Int]
-- {- evalD expr stack cont = evalK expr stack (applyCont cont) -}
evalD expr stack cont =
  case expr of
    Lit int ->
      -- evalK (Lit int) stack (applyCont cont)
      {- apply `evalK` -}
      applyCont cont (int : stack)
    Bin op e1 e2 ->
      evalD e1 stack (Exp e2 : Op op : cont)

applyCont :: [ContAction] -> [Int] -> [Int]
applyCont cont =
  case cont of
    [] -> id
    Op op : cont ->
      \s2 ->
        (applyCont cont) (evalOpS op s2)
    Exp e2 : cont' ->
      -- assert cont'@(ContOp op cont)
      \s1 -> evalD e2 s1 cont'

applyCont' :: [ContAction] -> [Int] -> [Int]
applyCont' cont =
  case cont of
    [] -> id
    Op op : cont ->
      applyCont' cont . evalOpS op
    Exp e2 : cont' ->
      -- assert cont'@(ContOp op cont)
      evalD' e2 cont'

evalD' :: Exp -> [ContAction] -> [Int] -> [Int]
-- {- evalD' expr cont stack = evalD expr stack cont -}
evalD' expr cont =
  case expr of
    Lit int ->
      applyCont' cont . (push int)
    Bin op e1 e2 ->
      evalD' e1 (Exp e2 : Op op : cont)

data Instr
  = End
  | IPush Int Instr
  | IOp Op Instr

applyContD :: [ContAction] -> Instr -> Instr
applyContD cont =
  case cont of
    [] -> id
    Op op : cont ->
      (applyContD cont) . (IOp op)
    Exp e2 : cont' ->
      -- assert cont'@(ContOp op cont)
      evalD'I e2 cont'

-- evalD' expr stack = applyInstr (evalD'I expr cont) stack
evalD'I :: Exp -> [ContAction] -> Instr -> Instr
evalD'I expr cont =
  case expr of
    Lit int ->
      -- applyCont' cont . (push int)
      applyContD cont . (IPush int)
    Bin op e1 e2 ->
      evalD'I e1 (Exp e2 : Op op : cont)
