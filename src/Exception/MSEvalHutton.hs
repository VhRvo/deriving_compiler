{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Evaluate" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Exception.MSEvalHutton where

import Exception.Def
import Exception.Step0Eval hiding (eval, evalK)
import Control.Applicative ((<|>))

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

evalS :: Exp -> [Int] -> Maybe [Int]
{-
evalS expr stack =
  case eval expr of
    Just v ->
      Just (v : stack)
    Nothing ->
      fail stack
-}
{-
evalS expr stack =
  maybe (fail stack) (Just . (: stack)) (eval expr)
-}
evalS expr stack =
  case expr of
    Lit int ->
      Just (int : stack)
    Bin op e1 e2 ->
      {- path 2-}
      -- (: stack) <$> (eval (Bin op e1 e2))
      -- (: stack) <$> (do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2))
      -- (: stack) <$>
      --   (case eval e1 of
      --     Nothing ->
      --       Nothing
      --     Just v1 ->
      --       case eval e2 of
      --         Nothing ->
      --           Nothing
      --         Just v2 ->
      --           Just (evalOp op v1 v2))
      -- case eval e1 of
      --   Nothing ->
      --     -- Nothing
      --     evalOpS op <$> Nothing
      --   Just v1 ->
      --     -- case eval e2 of
      --     --   Nothing ->
      --     --     -- Nothing
      --     --     evalOpS op <$> Nothing
      --     --   Just v2 ->
      --     --     -- Just (evalOp op v1 v2 : stack)
      --     --     -- Just (evalOpS op (v2 : v1 : stack))
      --     --     -- (Just . evalOpS op . (: (v1 : stack))) v2
      --     --     evalOpS op <$> ((Just . (: (v1 : stack))) v2)
      --     -- evalOpS op <$> (case eval e2 of
      --     --   Nothing ->
      --     --     Nothing
      --     --   Just v2 ->
      --     --     (Just . (: (v1 : stack))) v2)
      --     evalOpS op <$> (evalS e2 (v1 : stack))
      -- evalOpS op <$> case eval e1 of
      --   Nothing ->
      --     -- Nothing
      --     Nothing >>= evalS e2
      --   Just v1 ->
      --     -- evalS e2 (v1 : stack)
      --     Just (v1 : stack) >>= evalS e2
      -- evalOpS op <$>
      --   ((case eval e1 of
      --     Nothing ->
      --       Nothing
      --     Just v1 ->
      --       Just (v1 : stack)) >>= evalS e2)
      -- evalOpS op <$>
      --   ((case eval e1 of
      --     Nothing ->
      --       Nothing
      --     Just v1 ->
      --       ((Just . (: stack)) v1)) >>= evalS e2)
      -- evalOpS op <$>
      --   ((evalS e1 stack) >>= evalS e2)
      -- evalOpS op <$>
      --   ((evalS e1 stack) >>= evalS e2)
      -- evalOpS op <$>
      --   (case evalS e1 stack of
      --     Nothing ->
      --       Nothing
      --     Just s1 ->
      --       evalS e2 s1)
      case evalS e1 stack of
        Nothing ->
          -- evalOpS op <$> Nothing
          Nothing
        Just s1 ->
          -- evalOpS op <$> evalS e2 s1
          case evalS e2 s1 of
            Nothing ->
              -- evalOpS op <$> Nothing
              Nothing
            Just s2 ->
              -- evalOpS op <$> Just s2
              -- Just (evalOpS op s2)
              pure (evalOpS op s2)
    Throw ->
      Nothing
    Catch exception handler ->
      (evalS exception stack) <|> (evalS handler stack)

evalSIntuitive :: Exp -> [Int] -> Maybe [Int]
evalSIntuitive expr stack =
  case expr of
    Lit int ->
      pure (int : stack)
    Bin op e1 e2 -> do
      s1 <- evalSIntuitive e1 stack
      s2 <- evalSIntuitive e2 s1
      pure (evalOpS op s2)
    Throw ->
      Nothing
    Catch exception handler ->
      evalSIntuitive exception stack <|> evalSIntuitive handler stack


evalOpS :: Op -> [Int] -> [Int]
evalOpS op (v2 : v1 : stack) =
  evalOp op v1 v2 : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"
