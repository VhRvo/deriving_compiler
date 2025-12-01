{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Evaluate" #-}
module Exception.MSEval where

import Exception.Def
import Exception.Step0Eval hiding (eval, evalK)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))

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

evalSK :: Exp -> ([Int] -> Maybe [Int]) -> [Int] -> Maybe [Int]
{-
evalSK expr cont stack =
  case evalS expr stack of
    Just stack' ->
      cont stack'
    Nothing ->
      Nothing
-}
{-
evalSK expr cont stack =
  maybe Nothing cont (evalS expr stack)
-}
{-
evalSK expr cont stack = evalS expr stack >>= cont
-}
evalSK expr cont stack =
  case expr of
    Lit int ->
      -- evalS (Lit int) stack >>= cont
      -- Just (int : stack) >>= cont
      cont (int : stack)
    Bin op e1 e2 ->
      -- evalS (Bin op e1 e2) stack >>= cont
      -- (do
      --   s1 <- evalS e1 stack
      --   s2 <- evalS e2 s1
      --   pure (evalOpS op s2)) >>= cont
      -- (evalS e1 stack >>=
      --   (\s1 -> evalS e2 s1 >>=
      --     (\s2 -> pure (evalOpS op s2)))) >>= cont
      -- evalS e1 stack >>=
      --   ((\s1 -> evalS e2 s1 >>=
      --     (\s2 -> pure (evalOpS op s2))) >=> cont)
      -- evalS e1 stack >>=
      --   (\s1 -> evalS e2 s1 >>=
      --     (\s2 -> cont (evalOpS op s2)))
      -- evalSK e1
      --   (\s1 -> evalS e2 s1 >>=
      --     (\s2 -> cont (evalOpS op s2)))
      --   stack
      evalSK e1
        (\s1 ->
          evalSK e2
            (\s2 -> cont (evalOpS op s2))
            s1)
        stack
    Throw ->
      -- evalS Throw stack >>= cont
      -- Nothing >>= cont
      Nothing
    Catch exception handler ->
      -- evalS (Catch exception handler) stack >>= cont
      -- (evalS exception stack <|> evalS handler stack) >>= cont
      -- (evalS exception stack >>= cont) <|> (evalS handler stack >>= cont)
      (evalSK exception cont stack) <|> (evalSK handler cont stack)

evalS' :: Exp -> [Int] -> Maybe [Int]
{-
evalS' expr stack =
  case eval expr of
    Just v ->
      Just (v : stack)
    Nothing ->
      Nothing
-}
{-
evalS' expr stack =
  maybe Nothing (Just . (: stack)) (eval expr)
-}
{-
evalS' expr stack =
  (: stack) <$> (eval expr)
-}
{-
evalS' expr stack =
  (eval expr) >>= (Just . (: stack))
-}
evalS' expr stack =
  case expr of
    Lit int ->
      -- maybe Nothing (Just . (: stack)) (eval (Lit int))
      -- maybe Nothing (Just . (: stack)) (Just int)
      Just (int : stack)
    Bin op e1 e2 ->
      {- path 1 -}
      -- maybe Nothing (Just . (: stack)) (eval (Bin op e1 e2))
      -- maybe Nothing (Just . (: stack)) (do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2))
      -- maybe Nothing (Just . (: stack)) (
      --   case eval e1 of
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
      --     Nothing
      --   Just v1 ->
      --     case eval e2 of
      --       Nothing ->
      --         Nothing
      --       Just v2 ->
      --         -- Just ((evalOp op v1 v2) : stack)
      --         -- Just (evalOpS op (v2 : v1 : stack))
      --         (Just . evalOpS op . (: (v1 : stack))) v2

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
      --     evalOpS op <$> (evalS' e2 (v1 : stack))
      -- evalOpS op <$> case eval e1 of
      --   Nothing ->
      --     -- Nothing
      --     Nothing >>= evalS' e2
      --   Just v1 ->
      --     -- evalS' e2 (v1 : stack)
      --     Just (v1 : stack) >>= evalS' e2
      -- evalOpS op <$>
      --   ((case eval e1 of
      --     Nothing ->
      --       Nothing
      --     Just v1 ->
      --       Just (v1 : stack)) >>= evalS' e2)
      -- evalOpS op <$>
      --   ((case eval e1 of
      --     Nothing ->
      --       Nothing
      --     Just v1 ->
      --       ((Just . (: stack)) v1)) >>= evalS' e2)
      -- evalOpS op <$>
      --   ((evalS' e1 stack) >>= evalS' e2)
      -- evalOpS op <$>
      --   ((evalS' e1 stack) >>= evalS' e2)
      -- evalOpS op <$>
      --   (case evalS' e1 stack of
      --     Nothing ->
      --       Nothing
      --     Just s1 ->
      --       evalS' e2 s1)
      case evalS' e1 stack of
        Nothing ->
          -- evalOpS op <$> Nothing
          Nothing
        Just s1 ->
          -- evalOpS op <$> evalS' e2 s1
          case evalS' e2 s1 of
            Nothing ->
              -- evalOpS op <$> Nothing
              Nothing
            Just s2 ->
              -- evalOpS op <$> Just s2
              -- Just (evalOpS op s2)
              pure (evalOpS op s2)

      {- path 3-}
      -- eval (Bin op e1 e2) >>= (Just . (: stack))
      -- eval e1 >>= (\v1 ->
      --   eval e2 >>= (\v2 ->
      --     pure (evalOp op v1 v2))) >>= (Just . (: stack))
      -- eval e1 >>= (\v1 ->
      --   eval e2 >>= (\v2 ->
      --     (Just . (: stack)) (evalOp op v1 v2)))
      -- eval e1 >>= (\v1 ->
      --   eval e2 >>= (\v2 ->
      --     (Just (evalOp op v1 v2 : stack))))
      -- eval e1 >>= (\v1 ->
      --   eval e2 >>= (\v2 ->
      --     (Just (evalOpS op (v2 : v1 : stack)))))
      -- eval e1 >>= (\v1 ->
      --   eval e2 >>= (\v2 ->
      --     (Just . evalOpS op . (: v1 : stack)) v2))
      -- eval e1 >>= (\v1 ->
      --   eval e2 >>= (Just . evalOpS op . (: v1 : stack)))
      -- eval e1 >>= (\v1 ->
      --   evalOpS op <$> (eval e2 >>= (Just . (: v1 : stack))))
      -- eval e1 >>= (\v1 ->
      --   evalOpS op <$> (evalS' e2 (v1 : stack)))
    Throw ->
      maybe Nothing (Just . (: stack)) (eval Throw)
    Catch exception handler ->
      maybe Nothing (Just . (: stack)) (eval (Catch exception handler))


evalS :: Exp -> [Int] -> Maybe [Int]
evalS expr stack =
  case expr of
    Lit int ->
      pure (int : stack)
    Bin op e1 e2 -> do
      s1 <- evalS e1 stack
      s2 <- evalS e2 s1
      pure (evalOpS op s2)
    Throw ->
      Nothing
    Catch exception handler ->
      evalS exception stack <|> evalS handler stack
    --   (evalS exception <|> evalS handler) stack


evalOpS :: Op -> [Int] -> [Int]
evalOpS op (v2 : v1 : stack) =
  evalOp op v1 v2 : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"
