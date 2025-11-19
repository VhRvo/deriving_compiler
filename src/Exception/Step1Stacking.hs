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
      case eval e1 of
        Nothing -> Nothing : stack
        Just v1 ->
          case eval e2 of
            Nothing -> Nothing : stack
            Just v2 ->
              -- Just (evalOp op v1 v2) : stack
              {- unapply `evalOpS` -}
              evalOpS op (Just v2 : Just v1 : stack)

              {- unapply `eval` -}
              -- evalOpS op (eval e2 : Just v1 : stack)
              {- unapply specification -}
              -- evalOpS op (evalS e2 (Just v1 : stack))
              {- unapply `eval` -}
    Throw ->
      -- eval Throw : stack
      {- apply `eval` -}
      Nothing : stack
    Catch e1 e2 ->
      -- eval (Catch e1 e2) : stack
      {- apply `eval` -}
      case eval e1 of
        Nothing ->
          eval e2 : stack
        Just v1 ->
          Just v1 : stack

evalOpS :: Op -> Stack -> Stack
evalOpS op (Just v2 : Just v1 : stack) =
  Just (evalOp op v1 v2) : stack
evalOpS _ _ = error "unexpected use of evalOpS"
