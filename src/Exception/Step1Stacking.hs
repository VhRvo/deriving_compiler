module Exception.Step1Stacking where

import Exception.Def
import Exception.Step0Eval

data Element
  = Value Value
  | Exception

type Stack = [Element]

-- evalS expr stack = eval expr : stack
{-
evalS expr stack =
  case eval expr of
    Exception ->
      Exception : stack
    Value v1 ->
      Value v1 : stack
-}
evalS :: Exp -> Stack -> Stack
evalS expr stack =
  case expr of
    Lit int ->
      -- eval (Lit int) : stack
      {- apply `eval` -}
      Value int : stack
    Bin op e1 e2 ->
      -- eval (Bin op e1 e2) : stack
      {- apply `eval` -}
      -- case eval e1 of
      --   Exception ->
      --     -- Exception : stack
      --     evalOpS op (eval e2 : Exception : stack)
      --   Value v1 ->
      --     -- case eval e2 of
      --     --   Exception ->
      --     --     -- Exception : stack
      --     --     {- unapply `evalOpS` -}
      --     --     evalOpS op (Exception : Value v1 : stack)
      --     --   Value v2 ->
      --     --     -- Value (evalOp op v1 v2) : stack
      --     --     {- unapply `evalOpS` -}
      --     --     evalOpS op (Value v2 : Value v1 : stack)
      --     {- extract continuation -}
      --     evalOpS op (eval e2 : Value v1 : stack)
      {- extract continuation -}
      -- evalOpS op (eval e2 : eval e1 : stack)
      {- unapply specification -}
      -- evalOpS op (eval e2 : evalS e1 stack)
      {- unapply specification -}
      evalOpS op (evalS e2 (evalS e1 stack))
    Throw ->
      -- eval Throw : stack
      {- apply `eval` -}
      Exception : stack
    Catch e1 e2 ->
      {- method 1: incorrect semantics -}
      -- eval (Catch e1 e2) : stack
      {- apply `eval` -}
      -- case eval e1 of
      --   Exception ->
      --     -- eval e2 : stack
      --     {- unapply `evalCatchS` -}
      --     evalCatchS e2 (Exception : stack)
      --   Value v1 ->
      --     -- Value v1 : stack
      --     {- unapply `evalCatchS` -}
      --     evalCatchS e2 (Value v1 : stack)
      {- extract continuation -}
      -- evalCatchS e2 (eval e1 : stack)
      {- unapply specification -}
      -- evalCatchS e2 (evalS e1 stack)

      {- method 2 -}
      -- eval (Catch e1 e2) : stack
      {- apply `eval` -}
      -- case eval e1 of
      --   Exception ->
      --     -- eval e2 : stack
      --     {- unapply `evalCatchSHandler` -}
      --     evalCatchSHandler (eval e2 : stack) (Exception : stack)
      --   Value v1 ->
      --     -- Value v1 : stack
      --     {- unapply `evalCatchSHandler` -}
      --     evalCatchSHandler (eval e2 : stack) (Value v1 : stack)
      -- evalCatchSHandler (eval e2 : stack) (eval e1 : stack)
      {- unapply specification -}
      -- evalCatchSHandler (eval e2 : stack) (evalS e1 stack)
      {- unapply specification -}
      evalCatchSHandler (evalS e2 stack) (evalS e1 stack)

evalOpS :: Op -> Stack -> Stack
evalOpS op (Value v2 : Value v1 : stack) =
  Value (evalOp op v1 v2) : stack
evalOpS _ (Exception : _ : stack) =
  Exception : stack
evalOpS _ (_ : Exception : stack) =
  Exception : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"

evalCatchS :: Exp -> Stack -> Stack
evalCatchS _ (Value v1 : stack) = Value v1 : stack
evalCatchS e2 (Exception : stack) =
  -- incorrect semantics
  -- eval e2 : stack
  {- unapply specification -}
  evalS e2 stack
evalCatchS _ _ =
  error "unexpected use of evalOpS"

evalCatchSHandler :: Stack -> Stack -> Stack
evalCatchSHandler _ (Value v1 : stack) = Value v1 : stack
evalCatchSHandler handler (Exception : _) = handler
evalCatchSHandler _ _ =
  error "unexpected use of evalOpS"