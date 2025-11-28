{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use <|>" #-}
module Exception.Step0Eval where

import Exception.Def

type Value = Int

evalOp :: Op -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)

eval :: Exp -> Maybe Value
eval expr =
  case expr of
    Lit int ->
      Just int
    Bin op e1 e2 ->
      case eval e1 of
        Nothing ->
          Nothing
        Just v1 ->
          case eval e2 of
            Nothing ->
              Nothing
            Just v2 ->
              Just (evalOp op v1 v2)
    Throw ->
      Nothing
    Catch e1 e2 ->
      case eval e1 of
        Nothing ->
          eval e2
        Just v1 ->
          Just v1

evalK :: Exp -> (Maybe Value -> Maybe Value) -> Maybe Value
{- evalK expr cont = cont (eval expr) -}
{-
evalK expr cont =
  case expr of
    Nothing ->
      cnot Nothing
    Just v1 ->
      cnot (Just v1)
-}
evalK expr cont =
  case expr of
    Lit int ->
      cont (Just int)
    Bin op e1 e2 ->
      -- case eval e1 of
      --   Nothing ->
      --     -- cont Nothing
      --     {- unapply `maybe` -}
      --     -- cont (maybe Nothing (\v1 -> maybe Nothing (\v2 -> Just (evalOp op v1 v2)) (eval e2)) Nothing)
      --     {- unapply `composition` -}
      --     (cont . maybe Nothing (\v1 -> maybe Nothing (\v2 -> Just (evalOp op v1 v2)) (eval e2))) Nothing
      --   Just v1 ->
      --     -- case eval e2 of
      --     --   Nothing ->
      --     --     -- cont Nothing
      --     --     {- unapply `maybe` -}
      --     --     -- cont (maybe Nothing (\v2 -> Just (evalOp op v1 v2)) Nothing)
      --     --     {- unapply `composition` -}
      --     --     (cont . maybe Nothing (\v2 -> Just (evalOp op v1 v2))) Nothing
      --     --   Just v2 ->
      --     --     -- cont (Just (evalOp op v1 v2))
      --     --     {- unapply `maybe` -}
      --     --     -- cont (maybe Nothing (\v2 -> Just (evalOp op v1 v2)) (Just v2))
      --     --     {- unapply `composition` -}
      --     --     (cont . maybe Nothing (\v2 -> Just (evalOp op v1 v2))) (Just v2)
      --     {- extract continuation -}
      --     -- (cont . maybe Nothing (\v2 -> Just (evalOp op v1 v2))) (eval e2)
      --     {- apply `composition` -}
      --     -- cont (maybe Nothing (\v2 -> Just (evalOp op v1 v2)) (eval e2))
      --     {- unapply `maybe` -}
      --     -- cont (maybe Nothing (\v1 -> maybe Nothing (\v2 -> Just (evalOp op v1 v2)) (eval e2)) (Just v1))
      --     {- unapply `composition` -}
      --     (cont . maybe Nothing (\v1 -> maybe Nothing (\v2 -> Just (evalOp op v1 v2)) (eval e2))) (Just v1)
      {- extract continuation -}
      -- (cont . maybe Nothing (\v1 -> maybe Nothing (\v2 -> Just (evalOp op v1 v2)) (eval e2))) (eval e1)
      {- unapply specification -}
      -- (cont . maybe Nothing (\v1 -> evalK e2 (maybe Nothing (\v2 -> Just (evalOp op v1 v2))))) (eval e1)
      {- unapply specification -}
      evalK e1 (cont . maybe Nothing (\v1 -> evalK e2 (maybe Nothing (\v2 -> Just (evalOp op v1 v2)))))
    Throw ->
      cont Nothing
    Catch e1 e2 ->
      -- case eval e1 of
      --   Nothing ->
      --     -- cont (eval e2)
      --     {- unapply `maybe` -}
      --     -- cont (maybe (eval e2) Just Nothing)
      --     {- unapply `composition` -}
      --     (cont . maybe (eval e2) Just) Nothing
      --   Just v1 ->
      --     -- cont (Just v1)
      --     {- unapply `maybe` -}
      --     -- cont (maybe (eval e2) Just (Just v1))
      --     {- unapply `composition` -}
      --     (cont . maybe (eval e2) Just) (Just v1)
      {- extract continuation -}
      -- (cont . maybe (eval e2) Just) (eval e1)
      {- unapply specification -}
      -- evalK e1 (cont . maybe (eval e2) Just)

      -- path 1
      -- evalK e1 (cont . maybe (eval e2) Just)
      {- unapply `composition` -}
      -- (\result -> evalK e1 (cont . maybe result Just)) (eval e2)
      {- unapply specification -}
      -- evalK e2 (\result -> evalK e1 (cont . maybe result Just))

      -- path 2
      -- evalK e1 (cont . maybe (eval e2) Just)
      {- unapply eta -}
      -- evalK e1 (\r1 -> cont (maybe (eval e2) Just r1))
      {- extract continuation -}
      -- evalK e1 (\r1 -> (\r2 -> cont (maybe r2 Just r1)) (eval e2))
      {- apply specification -}
      evalK e1 (\r1 -> evalK e2 (\r2 -> cont (maybe r2 Just r1)))


eval2K :: Exp -> Value -> (Value -> Value) -> Value
{-
eval2K expr nothing just =
  maybe nothing just (eval expr)
  {- unapply `maybe` -}
  case eval expr of
    Nothing ->
      nothing
    Just v1 ->
      just v1
-}
eval2K expr nothing just =
  case expr of
    Lit int ->
      -- maybe nothing just (Just int)
      {- unapply `maybe` -}
      just int
    Bin op e1 e2 ->
      -- case eval e1 of
      --   Nothing ->
      --     -- maybe nothing just Nothing
      --     {- unapply `maybe` -}
      --     nothing
      --   Just v1 ->
      --     -- case eval e2 of
      --     --   Nothing ->
      --     --     -- maybe nothing just Nothing
      --     --     {- unapply `maybe` -}
      --     --     nothing
      --     --   Just v2 ->
      --     --     -- maybe nothing just (Just (evalOp op v1 v2))
      --     --     {- unapply `maybe` -}
      --     --     -- just (evalOp op v1 v2)
      --     --     {- unapply `composition` -}
      --     --     (just . evalOp op v1) v2
      --     {- unapply `maybe` -}
      --     -- maybe nothing (just . evalOp op v1) (eval e2)
      --     {- unapply `composition` -}
      --     (\v1 -> maybe nothing (just . evalOp op v1) (eval e2)) v1
      {- unapply `maybe` -}
      -- maybe nothing (\v1 -> maybe nothing (just . evalOp op v1) (eval e2)) (eval e1)
      {- unapply `eval2K` -}
      eval2K e1 nothing (\v1 -> maybe nothing (just . evalOp op v1) (eval e2))
    Throw ->
      -- maybe nothing just Nothing
      {- unapply `maybe` -}
      nothing
    Catch e1 e2 ->
      -- case eval e1 of
      --   Nothing ->
      --     maybe nothing just (eval e2)
      --   Just v1 ->
      --     -- maybe nothing just (Just v1)
      --     {- unapply `maybe` -}
      --     just v1
      {- apply `maybe` -}
      -- maybe (maybe nothing just (eval e2)) just (eval e1)
      {- unapply `eval2K` -}
      -- eval2K e1 (maybe nothing just (eval e2)) just
      {- unapply `eval2K` -}
      eval2K e1 (eval2K e2 nothing just) just

-- eval2K expr nothing just =
--   maybe nothing just (eval expr)
