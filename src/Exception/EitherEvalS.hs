{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
module Exception.EitherEvalS where

import Exception.Def
import Exception.Step0Eval hiding (eval, evalK)
import Control.Applicative ((<|>))
import Prelude hiding (fail)
import Data.Bifunctor (Bifunctor(first, second, bimap))

eval :: Exp -> Maybe Int
eval expr =
  case expr of
    Lit int ->
      pure int
    Bin op e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      pure (evalOp op v1 v2)
    Throw ->
      Nothing
    Catch exception handler ->
      eval exception <|> eval handler

evalOpS :: Op -> [Int] -> [Int]
evalOpS op (v2 : v1 : stack) =
  evalOp op v1 v2 : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"

evalS :: Exp -> [Int] -> Either [Int] [Int]
{-
evalS expr stack =
  case eval expr of
    Just v ->
      Right (v : stack)
    Nothing ->
      Left stack
-}
{-
evalS expr stack =
  maybe (Left stack) (Right . (: stack)) (eval expr)
-}
evalS expr stack =
  case expr of
    Lit int ->
      -- evalS (Lit int) stack
      -- maybe (Left stack) (Right . (: stack)) (eval (Lit int))
      -- maybe (Left stack) (Right . (: stack)) (Just int)
      Right (int : stack)
    Bin op e1 e2 ->
      -- evalS (Bin op e1 e2) stack
      -- maybe (Left stack) (Right . (: stack)) (eval (Bin op e1 e2))
      {- -}
      -- maybe (Left stack) (Right . (: stack)) $ do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2)
      {- -}
      -- maybe (Left stack) (Right . (: stack)) $ do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2)
      {- -}
      -- maybe (Left stack) (Right . (: stack)) $
      --   case eval e1 of
      --     Nothing ->
      --       Nothing
      --     Just v1 ->
      --       case eval e2 of
      --         Nothing ->
      --           Nothing
      --         Just v2 ->
      --           Just (evalOp op v1 v2)
      {- -}
      case eval e1 of
        Nothing ->
          -- maybe (Left stack) (Right . (: stack)) $
          --   Nothing
          -- Left stack
          {- -}
          first (evalOpS op) $
            Left stack
        Just v1 ->
          -- case eval e2 of
          --   Nothing ->
          --     -- maybe (Left stack) (Right . (: stack)) $
          --     --   Nothing
          --     -- Left stack
          --     fmap (evalOpS op) (Left stack)
          --   Just v2 ->
          --     -- maybe (Left stack) (Right . (: stack)) $
          --     --   Just (evalOp op v1 v2)
          --     {- -}
          --     -- Right (evalOp op v1 v2 : stack)
          --     {- -}
          --     -- Right (evalOpS op (v2 : v1 : stack))
          --     {- -}
          --     fmap (evalOpS op) (Right (v2 : (v1 : stack)))
          {- -}
          -- fmap (evalOpS op) $
          --   case eval e2 of
          --     Nothing ->
          --       -- Left (v1 : stack)
          --       tail `first` Left (v1 : stack)
          --     Just v2 ->
          --       -- Right (v2 : (v1 : stack))
          --       tail `first` Right (v2 : (v1 : stack))
          {- -}
          -- fmap (evalOpS op) $
          --   first tail $
          --     case eval e2 of
          --       Nothing ->
          --         Left (v1 : stack)
          --       Just v2 ->
          --         Right (v2 : (v1 : stack))
          {- -}
          -- fmap (evalOpS op) $
          --   first tail $
          --     evalS e2 (v1 : stack)
          {- -}
          fmap (evalOpS op) $
            first tail $
              evalS e2 (v1 : stack)
    Throw ->
      -- evalS Throw stack
      maybe (Left stack) (Right . (: stack)) (eval Throw)
    Catch exception handler ->
      -- evalS (Catch exception handler) stack
      maybe (Left stack) (Right . (: stack)) (eval (Catch exception handler))


