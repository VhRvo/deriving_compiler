module Exception.Step1Stacking where

import Exception.Def
import Exception.Step0Eval

type Stack = [Maybe Value]

-- evalS expr stack = eval expr : stack
{-
evalS expr stack =
  case eval expr of
    Nothing ->
      Nothing : stack
    Just v1 ->
      Just v1 : stack
-}
evalS :: Exp -> Stack -> Stack
evalS expr stack =
  case expr of
    Lit int ->
      -- eval (Lit int) : stack
      {- apply `eval` -}
      Just int : stack
    Bin op e1 e2 ->
      -- eval (Bin op e1 e2) : stack
      {- apply `eval` -}
      -- case eval e1 of
      --   Nothing ->
      --     -- Nothing : stack
      --     evalOpS op (eval e2 : Nothing : stack)
      --   Just v1 ->
      --     -- case eval e2 of
      --     --   Nothing ->
      --     --     -- Nothing : stack
      --     --     {- unapply `evalOpS` -}
      --     --     evalOpS op (Nothing : Just v1 : stack)
      --     --   Just v2 ->
      --     --     -- Just (evalOp op v1 v2) : stack
      --     --     {- unapply `evalOpS` -}
      --     --     evalOpS op (Just v2 : Just v1 : stack)
      --     {- extract continuation -}
      --     evalOpS op (eval e2 : Just v1 : stack)
      {- extract continuation -}
      -- evalOpS op (eval e2 : eval e1 : stack)
      {- unapply specification -}
      -- evalOpS op (eval e2 : evalS e1 stack)
      {- unapply specification -}
      evalOpS op (evalS e2 (evalS e1 stack))
    Throw ->
      -- eval Throw : stack
      {- apply `eval` -}
      Nothing : stack
    Catch e1 e2 ->
      {- method 1 -}
      -- eval (Catch e1 e2) : stack
      {- apply `eval` -}
      -- case eval e1 of
      --   Nothing ->
      --     -- eval e2 : stack
      --     {- unapply `evalCatchS` -}
      --     evalCatchS e2 (Nothing : stack)
      --   Just v1 ->
      --     -- Just v1 : stack
      --     {- unapply `evalCatchS` -}
      --     evalCatchS e2 (Just v1 : stack)
      {- extract continuation -}
      -- evalCatchS e2 (eval e1 : stack)
      {- unapply specification -}
      -- evalCatchS e2 (evalS e1 stack)

      {- method 2 -}
      -- eval (Catch e1 e2) : stack
      {- apply `eval` -}
      -- case eval e1 of
      --   Nothing ->
      --     -- eval e2 : stack
      --     {- unapply `evalCatchSHandler` -}
      --     evalCatchSHandler (eval e2 : stack) (Nothing : stack)
      --   Just v1 ->
      --     -- Just v1 : stack
      --     {- unapply `evalCatchSHandler` -}
      --     evalCatchSHandler (eval e2 : stack) (Just v1 : stack)
      -- evalCatchSHandler (eval e2 : stack) (eval e1 : stack)
      {- unapply specification -}
      -- evalCatchSHandler (eval e2 : stack) (evalS e1 stack)
      {- unapply specification -}
      evalCatchSHandler (evalS e2 stack) (evalS e1 stack)

evalOpS :: Op -> Stack -> Stack
evalOpS op (Just v2 : Just v1 : stack) =
  Just (evalOp op v1 v2) : stack
evalOpS _ (Nothing : _ : stack) =
  Nothing : stack
evalOpS _ (_ : Nothing : stack) =
  Nothing : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"

evalCatchS :: Exp -> Stack -> Stack
evalCatchS _ (Just v1 : stack) = Just v1 : stack
evalCatchS e2 (Nothing : stack) =
  -- eval e2 : stack
  {- unapply specification -}
  evalS e2 stack
evalCatchS _ _ =
  error "unexpected use of evalOpS"

evalCatchSHandler :: Stack -> Stack -> Stack
evalCatchSHandler _ (Just v1 : stack) = Just v1 : stack
evalCatchSHandler handler (Nothing : _) = handler
evalCatchSHandler _ _ =
  error "unexpected use of evalOpS"