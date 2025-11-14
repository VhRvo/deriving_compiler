{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use id" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Arith.Def where

import Prelude hiding (exp)

data Op
  = Add
  | Sub
  | Mul
  deriving (Eq, Ord, Show)

data Exp
  = Lit Int
  | Bin Op Exp Exp
  deriving (Eq, Ord, Show)

eval :: Exp -> Int
eval =
  \case
    Lit int -> int
    Bin op e1 e2 -> evalOp op (eval e1) (eval e2)

evalOp :: Op -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)

eval' :: Exp -> Int
eval' exp = evalK exp id

evalK :: Exp -> (Int -> Int) -> Int
{- evalK exp cont = cont (eval exp) -}
evalK exp cont =
  case exp of
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

data D1
  = D1End
  | D1Op Op Int D1
  | D1Exp Exp Op D1

eval'' :: Exp -> Int
eval'' exp = evalKD1 exp D1End

applyD1 :: D1 -> Int -> Int
applyD1 cont =
  case cont of
    D1End ->
      id
    D1Op op v1 cont ->
      \v2 -> (applyD1 cont) (evalOp op v1 v2)
    D1Exp e2 op cont ->
      \v1 -> evalKD1 e2 (D1Op op v1 cont)

evalKD1 :: Exp -> D1 -> Int
{- evalKD1 exp cont = evalK exp (applyD1 cont) -}
evalKD1 exp cont =
  case exp of
    Lit int ->
      evalK (Lit int) (applyD1 cont)
    Bin op e1 e2 ->
      -- evalK (Bin op e1 e2) (applyD1 cont)
      {- apply `evalK` -}
      -- (evalK e1) (\v1 -> (evalK e2) (\v2 -> (applyD1 cont) (evalOp op v1 v2)))
      {- introducing `D1Op` -}
      -- (evalK e1) (\v1 -> (evalK e2) (applyD1 (D1Op op v1 cont)))
      {- unapply `evalKD1` -}
      -- (evalK e1) (\v1 -> evalKD1 e2 (D1Op op v1 cont))
      {- introducing local binding  -}
      {- *** stuck, v1 is free variable but bounded by `D1Op` -}
      -- (evalK e1) (\v1 -> evalKD1 e2 (D1Op op v1 cont))
      {- introducing `D2Exp` -}
      -- (evalK e1) (applyD1 (D1Exp e2 op cont))
      {- unapply `evalKD1` -}
      evalKD1 e1 (D1Exp e2 op cont)

data D2
  = D2End
  | D2Exp Exp Op D2
  | D2Op Op Int D2

applyD2 :: D2 -> Int -> Int
applyD2 cont =
  case cont of
    D2End ->
      id
    D2Exp e2 op cont ->
      \v1 ->
        -- (evalK e2) (\v2 -> (applyD2 cont) (evalOp op v1 v2))
        {- introducing `D2Op` -}
        -- (evalK e2) (applyD2 (D2Op op v1 cont))
        {- unapply `evalKD2` -}
        (evalKD2 e2) (D2Op op v1 cont)
    D2Op op v1 cont ->
      \v2 -> (applyD2 cont) (evalOp op v1 v2)

evalKD2 :: Exp -> D2 -> Int
{- evalKD2 exp cont = evalK exp (applyD2 cont) -}
evalKD2 exp cont =
  case exp of
    Lit int ->
      evalK (Lit int) (applyD2 cont)
    Bin op e1 e2 ->
      -- evalK (Bin op e1 e2) (applyD2 cont)
      {- apply `evalK` -}
      -- (evalK e1) (\v1 -> (evalK e2) (\v2 -> (applyD2 cont) (evalOp op v1 v2)))
      {- introducing `D2Exp` -}
      -- (evalK e1) (applyD2 (D2Exp e2 op cont))
      {- unapply `evalKD2` -}
      (evalKD2 e1) (D2Exp e2 op cont)
