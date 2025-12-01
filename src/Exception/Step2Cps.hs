{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use lambda-case" #-}
module Exception.Step2Cps where

import Exception.Def
import Exception.Step0Eval
import Exception.Step1Stacking hiding (evalS, evalCatchSHandler, Exception, evalOpS, Value, Element(..), Stack)
import Prelude hiding (fail)


data Element
  = Value Value
  | Handler (Stack -> Stack)
  | Exception

type Stack = [Element]

evalS :: Exp -> Stack -> Stack
evalS expr stack =
  case expr of
    Lit int ->
      Value int : stack
    Bin op e1 e2 ->
      evalOpS op (evalS e2 (evalS e1 stack))
    Throw ->
      Exception : stack
    Catch e1 e2 ->
      evalCatchSHandler (evalS e2 stack) (evalS e1 stack)


evalSK :: Exp -> (Stack -> Stack) -> Stack -> Stack
{- evalSK expr cont stack = cont (evalS expr stack) -}
-- evalSK expr cont stack =
{- eta -}
evalSK expr cont =
  case expr of
    Lit int ->
      cont . (Value int :)
    Bin op e1 e2 ->
      -- cont (evalOpS op (evalS e2 (evalS e1 stack)))
      {- unapply `composition` -}
      -- (cont . evalOpS op) (evalS e2 (evalS e1 stack))
      {- unapply specification -}
      -- evalSK e2 (cont . evalOpS op) (evalS e1 stack)
      {- unapply specification -}
      evalSK e1 (evalSK e2 (cont . evalOpS op))
    Throw ->
      cont . (Exception :)
    Catch e1 e2 ->
      {- method 1 -}
      -- cont (evalCatchSHandler (evalS e2 stack) (evalS e1 stack))
      {- unapply `composition` -}
      -- (cont . evalCatchSHandler (evalS e2 stack)) (evalS e1 stack)
      {- unapply specification -}
      -- evalSK e1 (\s1 -> cont (evalCatchSHandler (evalS e2 stack) s1)) stack
      {- unapply `composition` -}
      -- evalSK e1 (\s1 -> (\s2 -> cont (evalCatchSHandler s2  s1)) (evalS e2 stack)) stack
      {- unapply specification -}
      -- \stack ->
      --   evalSK e1 (\s1 -> evalSK e2 (\s2 -> cont (evalCatchSHandler s2 s1)) stack) stack

      {- method 2 -}
      -- \stack ->
      --   cont (evalCatchSHandler (evalS e2 stack) (evalS e1 stack))
      {- apply `evalCatchSHandler` -}
      \stack ->
        -- case evalS e1 stack of
        --   (Value v1 : stack') ->
        --     cont (Value v1 : stack')
        --   Exception : stack' ->
        --     -- cont (evalS e2 stack)
        --     {- apply specification -}
        --     evalSK e2 cont stack'
        --   _ ->
        --     error "unexpected use of evalOpS"

        -- case evalS e1 (Handler stack : stack) of
        --   (Value v1 : Handler _ : stack') ->
        --     cont (Value v1 : stack')
        --   Exception : stack' ->
        --     -- cont (evalS e2 stack)
        --     {- apply specification -}
        --     evalSK e2 cont (fail stack')
        --   _ ->
        --     error "unexpected use of evalOpS"
        evalSK e1 (
          \stack ->
            case stack of
              (Value v1 : Handler cont : stack') ->
                cont (Value v1 : stack')
              Exception : stack' ->
                -- cont (evalS e2 stack)
                {- apply specification -}
                fail e2 stack'
              _ ->
                error "unexpected use of evalOpS"
          ) (Handler cont : stack)

fail :: Exp -> Stack -> Stack
fail _ [] = error "unexpected stack"
fail e2 (Value _ : stack) = fail e2 stack
fail e2 (Exception : stack) =
  fail e2 stack
fail e2 (Handler cont : stack) =
  evalSK e2 cont stack

data Code
  = Halt
  | Push Int Code
  | COp Op Code
  | Fail Code
  | CCatch Exp Exp Code

evalSKD :: Exp -> Code -> Code
evalSKD expr code =
  case expr of
    Lit int ->
      Push int code
    Bin op e1 e2 ->
      evalSKD e1 (evalSKD e2 (COp op code))
    Throw ->
      Fail code
    Catch e1 e2 ->
      CCatch e1 e2 code


evalOpS :: Op -> Stack -> Stack
evalOpS op (Value v2 : Value v1 : stack) =
  Value (evalOp op v1 v2) : stack
evalOpS _ (Exception : _ : stack) =
  Exception : stack
evalOpS _ (_ : Exception : stack) =
  Exception : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"

evalCatchSHandler :: Stack -> Stack -> Stack
evalCatchSHandler _ (Value v1 : stack) = Value v1 : stack
evalCatchSHandler handler (Exception : _) = handler
evalCatchSHandler _ _ =
  error "unexpected use of evalOpS"
