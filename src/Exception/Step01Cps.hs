{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Exception.Step01Cps where

import Exception.Def
import Exception.Step0Eval

data DK
  = DKId
  -- | DKRhs Op Int
  -- | DKLhs Exp DK DK
  | DKLhs Op Exp DK
  | DKRhs Op Int
  | DKHandler Exp DK
  | DKThrow (Maybe Int) DK

applyDK :: DK -> Maybe Value -> Maybe Value
applyDK dk =
  case dk of
    DKId ->
      id
    -- DKRhs op v1 ->
    --   maybe Nothing (\v2 -> Just (evalOp op v1 v2))
    -- DKLhs e2 dk rhsDk ->
    --   -- fail: cannot hide v1, cannot linearify v1
    --   applyDK dk . maybe Nothing (\v1 -> evalK e2 (applyDK rhsDk))
    DKLhs op e2 dk->
      -- applyDK dk . maybe Nothing (\v1 -> evalK e2 ((maybe Nothing (\v2 -> Just (evalOp op v1 v2)))))
      {- introducing `DKRhs` -}
      -- applyDK dk . maybe Nothing (\v1 -> evalK e2 (applyDK (DKRhs op v1)))
      {- unapply specification -}
      applyDK dk . maybe Nothing (\v1 -> evalKD e2 (DKRhs op v1))
    DKRhs op v1 ->
      maybe Nothing (\v2 -> Just (evalOp op v1 v2))
    DKHandler e1 dk ->
      -- \result -> evalK e1 (applyDK dk . maybe result Just)
      {- introducing `DKThrow` -}
      -- \result -> evalK e1 (applyDK (DKThrow result dk))
      {- unapply specification -}
      \result -> evalKD e1 (DKThrow result dk)
    DKThrow result dk ->
      applyDK dk . maybe result Just

evalKD :: Exp -> DK -> Maybe Value
{- evalKD exp dk = evalK exp (applyDK dk) -}
evalKD expr dk =
  case expr of
    Lit int ->
      -- evalKD (Lit int) dk
      {- apply specification -}
      -- evalK (Lit int) (applyDK dk)
      {- apply `evalK` -}
      applyDK dk (Just int)
    Bin op e1 e2 ->
      -- evalKD (Bin op e1 e2) dk
      {- apply specification -}
      -- evalK (Bin op e1 e2) (applyDK dk)
      {- apply `evalK` -}

      -- fail path: cannot linear v1
      -- evalK e1 (applyDK dk . maybe Nothing (\v1 -> evalK e2 ((maybe Nothing (\v2 -> Just (evalOp op v1 v2))))))
      -- evalK e1 ((applyDK dk . maybe Nothing (\v1 -> evalK e2 (applyDK (DKRhs op v1)))))
      -- evalK e1 ((applyDK dk . maybe Nothing (\v1 -> evalK e2 (applyDK (DKRhs op v1)))))

      -- another path
      -- evalK e1 ((applyDK dk . maybe Nothing (\v1 -> evalK e2 (maybe Nothing (\v2 -> Just (evalOp op v1 v2))))))
      {- introducing `DKLhs` -}
      -- evalK e1 (applyDK (DKLhs op e2 dk))
      {- unapply specification -}
      evalKD e1 (DKLhs op e2 dk)
    Throw ->
      -- evalKD Throw dk
      {- apply specification -}
      -- evalK Throw (applyDK dk)
      {- apply `evalK` -}
      applyDK dk Nothing
    Catch e1 e2 ->
      -- evalKD (Catch e1 e2) dk
      {- apply specification -}
      -- evalK (Catch e1 e2) (applyDK dk)
      {- apply `evalK` -}
      -- evalK e2 (\result -> evalK e1 (applyDK dk . maybe result Just))
      {- introducing `DKHandler` -}
      -- evalK e2 (applyDK (DKHandler e1 dk))
      {- unapply specification -}
      evalKD e2 (DKHandler e1 dk)
