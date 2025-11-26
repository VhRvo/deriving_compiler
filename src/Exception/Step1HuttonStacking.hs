module Exception.Step1HuttonStacking where

import Exception.Def
import Exception.Step0Eval

data Element
  = Value Int
  | Exception
type Stack = [Element]

{-
evalS expr stack =
  maybe (fail stack) (: stack . Value) (eval expr)
  maybe (fail stack) (\v -> Value v : stack) (eval expr)
-}
{-
evalS expr stack =
  case eval expr of
    Nothing ->
      fail stack
    Just v1 ->
      Value v1 : stack
-}
evalS :: Exp -> Stack -> Stack
evalS expr stack =
  case expr of
    Lit int ->
      -- evalS (Lit int) stack
      {- apply specification -}
      -- maybe (fail stack) (: stack . Value) (eval (Lit int))
      {- apply `eval` -}
      -- maybe (fail stack) (: stack . Value) (Just int)
      {- apply `maybe` -}
      Value int : stack
    Bin op e1 e2 ->
      -- evalS (Bin op e1 e2) stack
      {- apply specification -}
      -- maybe (fail stack) (: stack . Value) (eval (Bin op e1 e2))
      {- apply `eval` -}
      -- maybe (fail stack) (: stack . Value) $
      --   case eval e1 of
      --     Nothing ->
      --       Nothing
      --     Just v1 ->
      --       case eval e2 of
      --         Nothing ->
      --           Nothing
      --         Just v2 ->
      --           Just (evalOp op v1 v2)
      {- apply continuation -}
      case eval e1 of
        Nothing ->
          fail stack
        Just v1 ->
          case eval e2 of
            Nothing ->
              fail stack
              -- {- constructing `fail` -}
              -- fail (Exception : Value v1 : stack)
            Just v2 ->
              -- Value (evalOp op v1 v2) : stack
              {- constructing `evalOpS` -}
              evalOpS op (Value v2 : Value v1 : stack)
    Throw ->
      undefined
    Catch e1 e2 ->
      undefined

fail :: Stack -> Stack
fail (Exception : Value v1 : stack) =
  fail stack

evalOpS :: Op -> Stack -> Stack
evalOpS op (Value v2 : Value v1 : stack) =
  Value (evalOp op v1 v2) : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"