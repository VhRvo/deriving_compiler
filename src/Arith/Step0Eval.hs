{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Arith.Step0Eval where

import Arith.Def

eval :: Exp -> Int
eval =
  \case
    Lit int ->
      int
    Bin op e1 e2 ->
      evalOp op (eval e1) (eval e2)

evalOp :: Op -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)

evalK :: Exp -> (Int -> Int) -> Int
{- evalK expr cont = cont (eval expr) -}
evalK expr cont =
  case expr of
    Lit int ->
      -- cont (eval (Lit int))
      {- apply `eval` -}
      cont int
    Bin op e1 e2 ->
      -- cont (eval (Bin op e1 e2))
      {- apply `eval` -}
      -- cont (evalOp op (eval e1) (eval e2))
      {- extract surrounding context -}
      -- (\v1 -> cont (evalOp op v1 (eval e2))) (eval e1)
      {- unapply `evalK` -}
      -- (evalK e1) (\v1 -> cont (evalOp op v1 (eval e2)))
      {- extract surrounding context -}
      -- (evalK e1) (\v1 -> (\v2 -> cont (evalOp op v1 v2)) (eval e1))
      {- unapply `evalK` -}
      (evalK e1) (\v1 -> (evalK e2) (\v2 -> cont (evalOp op v1 v2)))

