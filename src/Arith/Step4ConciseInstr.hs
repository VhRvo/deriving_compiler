{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

module Arith.Step4ConciseInstr where

import Arith.Def
import Arith.Step1Stacking
import Prelude hiding (exp)

data Instr
  = Op Op
  | Exp Exp
  deriving (Eq, Ord, Show)

eval :: Exp -> [Int]
eval exp = evalD exp [] []

evalD :: Exp -> [Int] -> [Instr] -> [Int]
-- {- evalD exp stack cont = evalK exp stack (applyCont cont) -}
evalD exp stack cont =
  case exp of
    Lit int ->
      -- evalK (Lit int) stack (applyCont cont)
      {- apply `evalK` -}
      applyCont cont (int : stack)
    Bin op e1 e2 ->
      evalD e1 stack (Exp e2 : Op op : cont)

applyCont :: [Instr] -> [Int] -> [Int]
applyCont cont =
  case cont of
    [] -> id
    Op op : cont ->
      \s2 ->
        (applyCont cont) (evalOpS op s2)
    Exp e2 : cont' ->
      -- assert cont'@(ContOp op cont)
      \s1 -> evalD e2 s1 cont'

evalD' :: Exp -> [Instr] -> [Int] -> [Int]
-- {- evalD' exp cont stack = evalD exp stack cont -}
-- evalD' exp cont stack =
--   case exp of
--     Lit int ->
--       applyCont' cont (int : stack)
--     Bin op e1 e2 ->
--       evalD' e1 (Exp e2 : Op op : cont) stack
{- `eta` -}
evalD' exp cont =
  case exp of
    Lit int ->
      applyCont' cont . ((:) int)
    Bin op e1 e2 ->
      evalD' e1 (Exp e2 : Op op : cont)

applyCont' :: [Instr] -> [Int] -> [Int]
applyCont' cont =
  case cont of
    [] -> id
    Op op : cont ->
      -- \s2 ->
      --   (applyCont cont) (evalOpS op s2)
      {- `eta` -}
      (applyCont cont) . (evalOpS op)
    Exp e2 : cont' ->
      -- \s1 -> evalD' e2 cont' s1
      {- `eta` -}
      evalD' e2 cont'
