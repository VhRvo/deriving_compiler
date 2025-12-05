{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Exception.MEvalSEither where

import Exception.Def
import Exception.Step0Eval hiding (eval, evalK)
import Control.Applicative ((<|>))
import Prelude hiding (fail)

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

data Element
  = Value Int
  | Handler Exp
  deriving Show

evalOpS :: Op -> [Element] -> [Element]
evalOpS op (Value v2 : Value v1 : stack) =
  Value (evalOp op v1 v2) : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"

evalS1 :: Exp -> [Element] -> [Element]
evalS1 expr stack =
  case expr of
    Lit int ->
      Value int : stack
    Bin op e1 e2 ->
      evalOpS op (evalS1 e2 (evalS1 e1 stack))
    Throw ->
      fail stack
    Catch exception handler ->
      mark (evalS1 exception (Handler handler : stack))
      -- case evalS1 exception stack of
      --   Right stack' ->
      --     Right stack'
      --   Left _ ->
      --     evalS1 handler stack
  where
    fail :: [Element] -> [Element]
    fail (Value _ : stack) = fail stack
    fail (Handler handler : stack) = evalS1 handler stack
    fail [] = []

    mark :: [Element] -> [Element]
    mark = id
{-
-- failed
evalS :: Exp -> [Element] -> [Element]
-}

evalS :: Exp -> [Element] -> Either [Element] [Element]
{-
evalS expr stack =
  case eval expr of
    Just value ->
      success (Value value : stack)
    Nothing ->
      fail stack
-}
{-
evalS expr stack =
  maybe (fail stack) (success . (: stack) . Value) (eval expr)
-}
evalS expr stack =
  case expr of
    Lit int ->
      -- evalS (Lit int) stack
      -- maybe (fail stack) (success . (: stack) . Value) (eval (Lit int))
      success (Value int : stack)
    Bin op e1 e2 ->
      -- evalS (Bin op e1 e2) stack
      -- maybe (fail stack) (success . (: stack) . Value) (eval (Bin op e1 e2))
      {- -}
      -- maybe (fail stack) (success . (: stack) . Value) $ do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2)
      {- -}
      -- maybe (fail stack) (success . (: stack) . Value) $
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
      -- maybe (fail stack) (success . (: stack) . Value) $
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
          -- maybe (fail stack) (success . (: stack) . Value)
          --   Nothing
          {- -}
          fail stack
        Just v1 ->
          -- case eval e2 of
          --   Nothing ->
          --     -- maybe (fail stack) (success . (: stack) . Value)
          --     --   Nothing
          --     {- -}
          --     -- fail stack
          --     fail (Value v1 : stack)
          --   Just v2 ->
          --     -- maybe (fail stack) (success . (: stack) . Value) $
          --     --   Just (evalOp op v1 v2)
          --     {- -}
          --     -- success (Value (evalOp op v1 v2) : stack)
          --     {- -}
          --     -- success (evalOpS op (Value v2 : Value v1 : stack))
          --     {- -}
          --     (success . evalOpS op) (Value v2 : Value v1 : stack)
          -- maybe (fail (Value v1 : stack)) ((success . evalOpS op) . (: (Value v1 : stack)) . Value) (eval e2)
          maybe (fail (Value v1 : stack)) ((success . evalOpS op) . (: (Value v1 : stack)) . Value) (eval e2)
    Throw ->
      evalS Throw stack
      -- fail stack
    Catch exception handler ->
      evalS (Catch exception handler) stack
  where
    success :: [Element] -> Either [Element] [Element]
    success = Right
    fail :: [Element] -> Either [Element] [Element]
    fail (Value _ : stack) = fail stack


{-

fail :: [Element] -> Either [Element] [Element]
fail = undefined


evalS :: Exp -> [Element] -> Either [Element] [Element]
-- evalS expr stack =
--   maybe (fail stack) (Right . (: stack) . Value) (eval expr)
-- evalS expr stack =
--   case eval expr of
--     Just value ->
--       Right (Value value : stack)
--     Nothing ->
--       fail stack
evalS expr stack =
  case expr of
    Lit int ->
      -- evalS (Lit int) stack
      -- maybe (fail stack) (Right . (: stack)) (eval (Lit int))
      Right (Value int : stack)
    Bin op e1 e2 ->
      -- evalS (Bin op e1 e2) stack
      -- maybe (fail stack) (Right . (: stack) . Value) (eval (Bin op e1 e2))
      {- -}
      -- maybe (fail stack) (Right . (: stack) . Value) $ do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2)
      {- -}
      -- maybe (fail stack) (Right . (: stack) . Value) $
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
          -- maybe (fail stack) (Right . (: stack) . Value) Nothing
          fail stack
        Just v1 ->
          case eval e2 of
            Nothing ->
              -- maybe (fail stack) (Right . (: stack) . Value) Nothing
              fail stack
            Just v2 ->
              -- maybe (fail stack) (Right . (: stack) . Value) $
              --   Just (evalOp op v1 v2)
              -- Right (Value (evalOp op v1 v2) : stack)
              Right (evalOpS op (Value v2 : Value v1 : stack))
              -- failed
    Throw ->
      -- evalS Throw stack
      -- maybe (fail stack) (Right . (: stack) . Value) (eval Throw)
      fail stack
    Catch exception handler ->
      -- evalS (Catch exception handler) stack
      {- -}
      -- maybe (fail stack) (Right . (: stack) . Value) $
      --   eval exception <|> eval handler
      {- -}
      -- maybe (fail stack) (Right . (: stack) . Value) $
      --   case eval exception of
      --     Nothing ->
      --       eval handler
      --     Just v ->
      --       Just v
      {- -}
      -- case eval exception of
      --   Nothing ->
      --     -- maybe (fail stack) (Right . (: stack) . Value) $
      --     --   eval handler
      --     -- evalS handler stack
      --     fail (Handler handler : stack)
      --   Just v ->
      --     -- maybe (fail stack) (Right . (: stack) . Value) $
      --     --   Just v
      --     -- success (Value v : stack)
      --     success (Value v : Handler handler : stack)
      evalS exception (Handler handler : stack)
          -- failed
  where
    fail :: [Element] -> Either [Element] [Element]
    fail (Handler handler : stack) =
      evalS handler stack
    fail (Value v : stack) =
      fail stack
    fail [] =
      Left []
-}



{-
-- failed
evalS1 :: Exp -> [Element] -> Either [Element] [Element]
{-
evalS1 expr stack =
  maybe (Left (fail stack)) (Right . (: stack)) (eval expr)
-}
{-
evalS1 expr stack =
  case eval expr of
    Just value ->
      Right (value : stack)
    Nothing ->
      Left (fail stack)
-}
evalS1 expr stack =
  case expr of
    Lit int ->
      -- evalS1 (Lit int) stack
      -- maybe (Left (fail stack)) (Right . (: stack)) (eval (Lit int))
      -- maybe (Left (fail stack)) (Right . (: stack)) (pure int)
       Right (Value int : stack)
    Bin op e1 e2 ->
      -- evalS1 (Bin op e1 e2) stack
      -- maybe (Left (fail stack)) (Right . (: stack)) (eval (Bin op e1 e2))
      {- -}
      -- maybe (Left (fail stack)) (Right . (: stack)) $ do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2)
      {- -}
      -- maybe (Left (fail stack)) (Right . (: stack)) $
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
      -- case eval e1 of
      --   Nothing ->
      --     -- maybe (Left (fail stack)) (Right . (: stack)) Nothing
      --     -- Left (fail stack)
      --     evalOpS op <$> Left (fail stack)
      --   Just v1 ->
      --     -- case eval e2 of
      --     --   Nothing ->
      --     --     -- maybe (Left (fail stack)) (Right . (: stack)) Nothing
      --     --     -- Left (fail stack)
      --     --     -- Left (fail (v1 : stack))
      --     --     {- -}
      --     --     evalOpS op <$> Left (fail (v1 : stack))
      --     --   Just v2 ->
      --     --     -- maybe (Left (fail stack)) (Right . (: stack)) $
      --     --     --   Just (evalOp op v1 v2)
      --     --     {- -}
      --     --     -- (evalOp op v1 v2) : stack
      --     --     {- -}
      --     --     -- evalOpS op (v2 : v1 : stack)
      --     --     {- -}
      --     --     -- Right (evalOpS op . (: (v1 : stack)) $ v2)
      --     --     {- -}
      --     --     evalOpS op <$> Right ((: (v1 : stack)) v2)
      --     {- -}
      --     evalOpS op <$>
      --       -- case eval e2 of
      --       --   Nothing ->
      --       --     Left (fail (v1 : stack))
      --       --   Just v2 ->
      --       --     Right ((: (v1 : stack)) v2)
      --       {- -}
      --       evalS1 e2 (v1 : stack)
      {- -}
      -- fmap (evalOpS op) $
      --   case eval e1 of
      --     Nothing ->
      --       -- Left (fail stack)
      --       Left (fail stack) >>= evalS1 e2
      --     Just v1 ->
      --       -- evalS1 e2 (v1 : stack)
      --       Right (v1 : stack) >>= evalS1 e2
      {- -}
      -- fmap (evalOpS op) $
      --   (>>= evalS1 e2) $
      --     evalS1 e1 stack
      {- -}
      -- fmap (evalOpS op) $
      --   (>>= evalS1 e2) $
      --     evalS1 e1 stack
      {- -}
      -- fmap (evalOpS op) $
      --     evalS1 e1 stack >>= evalS1 e2
      {- -}
      -- evalS1 e1 stack >>= fmap (evalOpS op) . evalS1 e2
      {- -}
      evalS1 e1 stack >>= evalS1 e2 >>= (Right . evalOpS op)
    Throw ->
      -- evalS1 Throw stack
      -- maybe (Left (fail stack)) (Right . (: stack)) (eval Throw)
      Left (fail stack)
    Catch exception handler ->
      -- evalS1 (Catch exception handler) stack
      {- -}
      -- maybe (Left (fail stack)) (Right . (: stack)) $
      --   eval exception <|> eval handler
      {- -}
      -- maybe (Left (fail stack)) (Right . (: stack)) $
      --   case eval exception of
      --     Nothing ->
      --       eval handler
      --     Just v ->
      --       Just v
      {- -}
      case eval exception of
        Nothing ->
          -- maybe (Left (fail stack)) (Right . (: stack)) $
          --   eval handler
          {- -}
          -- evalS1 handler stack
          {- fail (handler : stack) = evalS1 handler stack -}
          Left (fail (Handler handler : stack))
        Just v ->
          -- maybe (Left (fail stack)) (Right . (: stack)) $
          --   Just v
          {- -}
          Right (Value v : stack)

data Element
  = Value Int
  | Handler Exp
  deriving Show

fail :: [Element] -> [Element]
fail = undefined

evalOpS :: Op -> [Element] -> [Element]
evalOpS op (Value v2 : Value v1 : stack) =
  Value (evalOp op v1 v2) : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"
-}

