{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Evaluate" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Redundant $" #-}
module Exception.MSEval where

import Exception.Def
import Exception.Step0Eval hiding (eval, evalK)
import Control.Applicative ((<|>))
import Prelude hiding (fail)
-- import Control.Monad ((>=>))

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

-- exec (comp' expr cont) stack =
--   evalSK expr (exec cont) stack

data Cont
  = Push Int Cont
  | EvalOp Op Cont
  | Fail
  | Halt
  | ReaderAlter Cont Cont
  -- | CCatch Exp Cont
  -- | CHandler Exp Cont

exec :: Cont -> [Element] -> Maybe [Element]
exec cont stack =
  case cont of
    Halt ->
      Just stack
    Push int cont ->
      exec cont (Value int : stack)
    Fail ->
      Nothing
    EvalOp op cont ->
      exec cont (evalOpS op stack)
    ReaderAlter exception hanlder ->
      case exec exception stack of
        Nothing ->
          exec hanlder stack
        Just stack' ->
          Just stack'

comp :: Exp -> Cont
comp expr = comp' expr Halt

comp' :: Exp -> Cont -> Cont
comp' expr cont =
  case expr of
    Lit int ->
      Push int cont
    Bin op e1 e2 ->
      comp' e1 (comp' e2 (EvalOp op cont))
    Throw ->
      Fail
    Catch exception handler ->
      -- \stack ->
      --   (evalSK exception cont stack) <|> (evalSK handler cont stack)
      {- -}
      -- \stack ->
      --   readerAlter (\stack -> evalSK exception cont stack) (\stack -> evalSK handler cont stack) stack
      {- -}
      ReaderAlter
        (comp' exception cont)
        (comp' handler cont)

readerAlter :: ([Element] -> Maybe [Element]) -> ([Element] -> Maybe [Element]) -> [Element] -> Maybe [Element]
readerAlter f g stack =
  case f stack of
    Nothing ->
      g stack
    Just stack' ->
      Just stack'

evalSK :: Exp -> ([Element] -> Maybe [Element]) -> [Element] -> Maybe [Element]
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
      cont (Value int : stack)
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
      {- -}
      (evalSK exception cont stack) <|> (evalSK handler cont stack)
      {- -}
      -- case evalSK exception cont stack of
      --   Nothing ->
      --     evalSK handler cont stack
      --   Just stack' ->
      --     Just stack'
      {- -}
      -- readerAlter
      --   (\stack -> evalSK exception cont stack)
      --   (\stack -> evalSK handler cont stack) stack
      {- -}
      -- readerAlter
      --   (evalSK exception cont)
      --   (evalSK handler cont) stack

data Element
  = Value Int
  | Handler Exp

evalS :: Exp -> [Element] -> Maybe [Element]
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
      -- maybe Nothing (Just . (: stack)) (eval (Lit int))
      -- maybe Nothing (Just . (: stack)) (Just int)
      Just (Value int : stack)
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
      --   evalOpS op <$> (evalS e2 (v1 : stack)))
    Throw ->
      -- maybe Nothing (Just . (: stack)) (eval Throw)
      -- maybe Nothing (Just . (: stack)) Nothing
      Nothing
    Catch exception handler ->
      -- maybe Nothing (Just . (: stack)) (eval (Catch exception handler))
      -- maybe Nothing (Just . (: stack)) (eval exception <|> eval handler)
      -- (: stack) <$> (eval exception <|> eval handler)
      -- ((: stack) <$> eval exception) <|> ((: stack) <$> eval handler)
      -- (evalS exception stack) <|> (evalS handler stack)
      {- -}
      -- case evalS exception stack of
      --   Nothing ->
      --     -- evalS handler stack
      --     failException (Handler handler : stack)
      --   Just stack' ->
      --     Just stack'
      -- maybe Nothing (Just . (: stack)) (eval (Catch exception handler))
      -- maybe Nothing (Just . (: stack)) (eval exception <|> eval handler)
      -- maybe Nothing (Just . (: stack)) $
      --   case eval exception of
      --     Nothing ->
      --       eval handler
      --     Just value ->
      --       Just value
      {- -}
      -- case eval exception of
      --   Nothing ->
      --     -- maybe Nothing (Just . (: stack)) $
      --     --   eval handler
      --     -- evalS handler stack
      --     handleHandler (Handler handler : stack)
      --   Just value ->
      --     -- maybe Nothing (Just . (: stack)) $
      --     --   Just value
      --     -- Just (Value value : stack)
      --     handleHandler (Value value : Handler handler : stack)
      {- -}
      handleHandler $
        case eval exception of
          Nothing ->
            Handler handler : stack
          Just value ->
            Value value : Handler handler : stack



handleHandler :: [Element] -> Maybe [Element]
handleHandler (Value value : Handler handler: stack) =
  Just (Value value : stack)
handleHandler (Handler handler : stack) =
  evalS handler stack



evalSIntuitive :: Exp -> [Element] -> Maybe [Element]
evalSIntuitive expr stack =
  case expr of
    Lit int ->
      pure (Value int : stack)
    Bin op e1 e2 -> do
      s1 <- evalSIntuitive e1 stack
      s2 <- evalSIntuitive e2 s1
      pure (evalOpS op s2)
    Throw ->
      Nothing
    Catch exception handler ->
      evalSIntuitive exception stack <|> evalSIntuitive handler stack

evalOpS :: Op -> [Element] -> [Element]
evalOpS op (Value v2 : Value v1 : stack) =
  Value (evalOp op v1 v2) : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"

evalSK2 :: Exp -> [Element] -> ([Element] -> [Element]) -> [Element] -> [Element]
{-
evalSK2 expr fail success stack =
  case evalS expr stack of
    Just stack' ->
      success stack'
    Nothing ->
      fail
-}
{-
evalSK2 expr fail success stack =
  maybe fail success (evalS expr stack)
-}
evalSK2 expr fail success stack =
  case expr of
    Lit int ->
      -- evalSK2 (Lit int) fail success stack
      -- maybe fail success (evalS (Lit int) stack)
      -- maybe fail success (int : stack)
      success (Value int : stack)
    Bin op e1 e2 ->
      -- evalSK2 (Bin op e1 e2) fail success stack
      -- maybe fail success (evalS (Bin op e1 e2) stack)
      {- -}
      -- maybe fail success $ do
      --   s1 <- evalS e1 stack
      --   s2 <- evalS e2 s1
      --   pure (evalOpS op s2)
      {- -}
      -- maybe fail success $
      --   case evalS e1 stack of
      --     Nothing ->
      --       Nothing
      --     Just s1 ->
      --       case evalS e2 s1 of
      --         Nothing ->
      --           Nothing
      --         Just s2 ->
      --           pure (evalOpS op s2)
      {- -}
      -- case evalS e1 stack of
      --   Nothing ->
      --     -- maybe fail success $
      --     --   Nothing
      --     {- -}
      --     fail
      --   Just s1 ->
      --     -- case evalS e2 s1 of
      --     --   Nothing ->
      --     --     -- maybe fail success $
      --     --     --   Nothing
      --     --     {- -}
      --     --     fail
      --     --   Just s2 ->
      --     --     -- maybe fail success $
      --     --     --   Just (evalOpS op s2)
      --     --     {- -}
      --     --     -- success (evalOpS op s2)
      --     --     {- -}
      --     --     (success . evalOpS op) s2
      --     evalSK2 e2 fail (success . evalOpS op) s1
      evalSK2 e1 fail (evalSK2 e2 fail (success . evalOpS op)) stack
    Throw ->
      fail
    Catch exception handler ->
      -- maybe fail success $
      --   case evalS exception stack of
      --     Nothing ->
      --       evalS handler stack
      --     Just stack' ->
      --       Just stack'
      {- -}
      -- case evalS exception stack of
      --   Nothing ->
      --     maybe fail success $
      --       evalS handler stack
      --   Just stack' ->
      --     maybe fail success $
      --       Just stack'
      {- -}
      -- case evalS exception stack of
      --   Nothing ->
      --     -- maybe fail success $
      --     --   evalS handler stack
      --     {- -}
      --     evalSK2 handler fail success stack
      --   Just stack' ->
      --     -- maybe fail success $
      --     --   Just stack'
      --     {- -}
      --     success stack'
      {- -}
      -- maybe
      --   (evalSK2 handler fail success stack)
      --   success
      --   (evalS exception stack)
      {- -}
      evalSK2 exception
        (evalSK2 handler fail success stack)
        success
        stack

-- evalSK2D :: Exp -> [Element] -> ([Element] -> [Element]) -> [Element] -> [Element]
