{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Exception.Step2Cps where

import Exception.Def
import Exception.Step0Eval
import Exception.Step1Stacking

evalSK :: Exp -> (Stack -> Stack) -> Stack -> Stack
{- evalSK expr cont stack = cont (evalS expr stack) -}
evalSK expr cont stack =
  case expr of
    Lit int ->
      cont (Just int : stack)
    Bin op e1 e2 ->
      -- cont (evalOpS op (evalS e2 (evalS e1 stack)))
      {- unapply `composition` -}
      -- (cont . evalOpS op) (evalS e2 (evalS e1 stack))
      {- unapply specification -}
      -- evalSK e2 (cont . evalOpS op) (evalS e1 stack)
      {- unapply specification -}
      evalSK e1 (evalSK e2 (cont . evalOpS op)) stack
    Throw ->
      cont (Nothing : stack)
    Catch e1 e2 ->
      -- cont (evalCatchSHandler (evalS e2 stack) (evalS e1 stack))
      {- unapply `composition` -}
      -- (cont . evalCatchSHandler (evalS e2 stack)) (evalS e1 stack)
      {- unapply specification -}
      -- evalSK e1 (\s1 -> cont (evalCatchSHandler (evalS e2 stack) s1)) stack
      {- unapply `composition` -}
      -- evalSK e1 (\s1 -> (\s2 -> cont (evalCatchSHandler s2  s1)) (evalS e2 stack)) stack
      {- unapply specification -}
      evalSK e1 (\s1 -> evalSK e2 (\s2 -> cont (evalCatchSHandler s2 s1)) stack) stack
