{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
module Exception.EitherEvalSK where

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

evalS :: Exp -> [Int] -> Maybe [Int]
{-
evalS expr stack =
  case eval expr of
    Just v ->
      Just (v : stack)
    Nothing ->
      Nothing
-}
{-
evalS expr stack =
  maybe Nothing (Just . (: stack)) (eval expr)
-}
{-
evalS expr stack =
  (: stack) <$> (eval expr)
-}
{-
evalS expr stack =
  (eval expr) >>= (Just . (: stack))
-}
evalS expr stack =
  case expr of
    Lit int ->
      Just (int : stack)
    Bin op e1 e2 ->
      case evalS e1 stack of
        Nothing ->
          Nothing
        Just s1 ->
          case evalS e2 s1 of
            Nothing ->
              Nothing
            Just s2 ->
              pure (evalOpS op s2)
    Throw ->
      Nothing
    Catch exception handler ->
      case evalS exception stack of
        Nothing ->
          evalS handler stack
        Just stack' ->
          Just stack'

