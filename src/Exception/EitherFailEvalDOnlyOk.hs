{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exception.EitherFailEvalDOnlyOk where

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

evalOpS :: Op -> [Element] -> [Element]
evalOpS op (Value v2 : Value v1 : stack) =
  Value (evalOp op v1 v2) : stack
evalOpS _ _ =
  error "unexpected use of evalOpS"

data Element
  = Value Int
  | Handler Exp
  deriving (Eq, Ord, Show)

evalS :: Exp -> [Element] -> Either [Element] [Element]
{-
evalS expr stack =
  case eval expr of
    Nothing ->
      Left (fail stack)
    Just v ->
      Right (v : stack)
-}
{-
evalS expr stack =
  maybe (Left (fail stack)) (Right . (: stack) . Value) (eval expr)
-}
evalS expr stack =
  case expr of
    Lit int ->
      -- evalS (Lit int) stack
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) (eval (Lit int))
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) (Just int)
      Right (Value int : stack)
    Bin op e1 e2 ->
      -- evalS (Bin op e1 e2) stack
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) (eval (Bin op e1 e2))
      {- -}
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) $ do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2)
      {- -}
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) $ do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2)
      {- -}
      maybe (Left (fail stack)) (Right . (: stack) . Value) $
        case eval e1 of
          Nothing ->
            Nothing
          Just v1 ->
            case eval e2 of
              Nothing ->
                Nothing
              Just v2 ->
                Just (evalOp op v1 v2)
      {- -}
      -- case eval e1 of
      --   Nothing ->
      --     -- maybe (Left (fail stack)) (Right . (: stack) . Value) $
      --     --   Nothing
      --     -- Left (fail stack)
      --     evalOpS op <$> Left (fail stack)
      --   Just v1 ->
      --     -- case eval e2 of
      --     --   Nothing ->
      --     --     -- maybe (Left (fail stack)) (Right . (: stack) . Value) $
      --     --     --   Nothing
      --     --     -- Left (fail stack)
      --     --     fmap (evalOpS op) (Left (fail stack))
      --     --   Just v2 ->
      --     --     -- maybe (Left (fail stack)) (Right . (: stack) . Value) $
      --     --     --   Just (evalOp op v1 v2)
      --     --     {- -}
      --     --     -- Right (Value (evalOp op v1 v2) : stack)
      --     --     {- -}
      --     --     -- Right (evalOpS op (Value v2 : Value v1 : stack))
      --     --     {- -}
      --     --     fmap (evalOpS op) (Right (Value v2 : (Value v1 : stack)))
      --     {- -}
      --     -- fmap (evalOpS op) $
      --     --   case eval e2 of
      --     --     Nothing ->
      --     --       -- Left (fail stack)
      --     --       {- -}
      --     --       Left (fail (Value v1 : stack))
      --     --     Just v2 ->
      --     --       Right (Value v2 : (Value v1 : stack))
      --     {- -}
      --     evalOpS op <$>
      --       evalS e2 (Value v1 : stack)
      {- -}
      -- fmap (evalOpS op) $
      --   case eval e1 of
      --     Nothing ->
      --       -- Left (fail stack)
      --       Left (fail stack) >>= evalS e2
      --     Just v1 ->
      --       -- evalS e2 (Value v1 : stack)
      --       Right (Value v1 : stack) >>= evalS e2
      {- -}
      -- fmap (evalOpS op) $
      --   (>>= evalS e2) $
      --     case eval e1 of
      --       Nothing ->
      --         Left (fail stack)
      --       Just v1 ->
      --         Right (Value v1 : stack)
      {- -}
      -- fmap (evalOpS op) $
      --   (>>= evalS e2) $
      --     evalS e1 stack
      {- -}
      -- fmap (evalOpS op) $
      --   evalS e1 stack >>= evalS e2
      {- -}
      -- evalS e1 stack >>= evalS e2 >>= Right . evalOpS op
    Throw ->
      -- evalS Throw stack
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) (eval Throw)
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) Nothing
      Left (fail stack)
    Catch exception handler ->
      -- evalS (Catch exception handler) stack
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) (eval (Catch exception handler))
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) (eval exception <|> eval handler)
      {- -}
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) $
      --   case eval exception of
      --     Nothing ->
      --       eval handler
      --     Just value ->
      --       Just value
      {- -}
      -- case eval exception of
      --   Nothing ->
      --     -- maybe (Left (fail stack)) (Right . (: stack) . Value) $
      --     --   eval handler
      --     {- -}
      --     -- evalS handler stack
      --     handleHandler (Left (Handler handler : stack))
      --     {- -}
      --     -- Left (Handler handler : stack) `errBind` failException
      --     -- Left (fail (Handler handler : stack)) `errBind` failException
      --   Just value ->
      --     -- maybe (Left (fail stack)) (Right . (: stack) . Value) $
      --     --   Just value
      --     -- Right (Value value : stack)
      --     handleHandler (Right (Value value : Handler handler : stack))
      --     -- ignoreHandler <$> Right (Value value : Handler handler : stack)
      {- -}
      -- handleHandler $
      --   case eval exception of
      --     Nothing ->
      --       -- Left (Handler handler : stack)
      --       Left (fail (Handler handler : stack))
      --     Just value ->
      --       Right (Value value : Handler handler : stack)
      {- -}
      -- handleHandler $
      --   case eval exception of
      --     Nothing ->
      --       -- Left (Handler handler : stack)
      --       Left (fail (Handler handler : stack))
      --     Just value ->
      --       Right (Value value : Handler handler : stack)
      {- -}
      handleHandler $
        evalS exception (Handler handler : stack)

      {- -}
      -- fmap ignoreHandler $
      --   (`errBind` failException) $
      --     case eval exception of
      --       Nothing ->
      --         Left (fail (Handler handler : stack))
      --       Just value ->
      --         Right (Value value : Handler handler : stack)
      {- -}
      -- fmap ignoreHandler $
      --   (`errBind` failException) $
      --     evalS exception (Handler handler : stack)
      {- -}
      -- fmap ignoreHandler $
      --   (`errBind` failException) $
      --     case evalS exception (Handler handler : stack) of
      --       Left (Handler hanlder : stack) ->
      --         Left (fail (Handler hanlder : stack))
      --       Right (Value v : Handler handler : stack) ->
      --         Right (Value v : Handler handler : stack)
      {- -}
      -- case evalS exception (Handler handler : stack) of
      --   Left (Handler handler : stack) ->
      --     evalS handler stack
      --   Right (Value value : Handler handler : stack) ->
      --     Right (Value value : stack)
      -- {- -}
      -- -- (`errBind` evalS handler) $
      -- --   case eval exception of
      -- --     Nothing ->
      -- --       Left stack
      -- --     Just value ->
      -- --       Right (Value value : stack)
      -- {- -}
      -- -- (`errBind` evalS handler) $
      -- --   evalS exception stack
      -- {- -}
      -- evalS exception stack `errBind` evalS handler

fail :: [Element] -> [Element]
fail [] = []
fail (Value _ : stack) = fail stack
fail (Handler handler : stack) = Handler handler : stack

handleHandler :: Either [Element] [Element] -> Either [Element] [Element]
handleHandler (Left (Handler handler : stack)) =
  evalS handler stack
handleHandler (Right (Value value : Handler handler : stack)) =
  Right (Value value : stack)

{- error calculating

evalSK :: Exp -> ([Element] -> [Element]) -> [Element] -> [Element]
{-
evalSK expr contOk stack =
  case eval expr of
    Nothing ->
      fail stack
    Just v ->
      contOk (Value v : stack)
-}
{-
evalSK expr contOk stack =
  maybe (fail stack) (contOk . (: stack) . Value) (eval expr)
-}
evalSK expr contOk =
  case expr of
    Lit int ->
      contOk . (Value int :)
    Bin op e1 e2 ->
      evalSK e1 (evalSK e2 (contOk . evalOpS op))
    Throw ->
      fail
    Catch exception handler ->
      evalSK exception (handleHandlerK contOk) . (Handler handler :)

handleHandlerK :: ([Element] -> [Element]) -> [Element] -> [Element]
handleHandlerK contOk (Handler handler : stack) =
  evalSK handler contOk stack
handleHandlerK contOk (Value value : Handler handler : stack) =
  contOk (Value value : stack)

data Code
  = Halt
  | Push Int Code
  | Op Op Code
  | Fail
  | Handle Code
  | CCatch Code Exp

evalSKD :: Exp -> Code -> Code
evalSKD expr contOk =
  case expr of
    Lit int ->
      -- contOk . (Value int :)
      Push int contOk
    Bin op e1 e2 ->
      -- evalSK e1 (evalSK e2 (contOk . evalOpS op))
      evalSKD e1 (evalSKD e2 (Op op contOk))
    Throw ->
      -- fail
      Fail
    Catch exception handler ->
      -- evalSK exception (handleHandlerK contOk) . (Handler handler :)
      -- evalSKD exception (Handle contOk) . (Handler handler :)
      CCatch (evalSKD exception (Handle contOk)) handler

applyCode :: Code -> [Element] -> [Element]
applyCode Halt                        stack = stack
applyCode (Push int contOk)           stack = applyCode contOk (Value int : stack)
applyCode (Op op contOk)              stack = applyCode contOk (evalOpS op stack)
applyCode Fail                        stack = fail stack
applyCode (Handle contOk)             stack = handleHandlerD contOk stack
applyCode (CCatch exception' handler) stack = applyCode exception' (Handler handler : stack)

handleHandlerD :: Code -> [Element] -> [Element]
handleHandlerD contOk (Handler handler : stack) =
  applyCode (evalSKD handler contOk) stack
handleHandlerD contOk (Value value : Handler handler : stack) =
  applyCode contOk (Value value : stack)
-}

data ElementK
  = ValueK Int
  | HandlerK ([ElementK] -> [ElementK])

evalSK :: Exp -> ([ElementK] -> [ElementK]) -> [ElementK] -> [ElementK]
{-
evalSK expr contOk stack =
  case eval expr of
    Nothing ->
      failSK stack
    Just v ->
      contOk (ValueK v : stack)
-}
{-
evalSK expr contOk stack =
  maybe (failSK stack) (contOk . (: stack) . ValueK) (eval expr)
-}
evalSK expr contOk stack =
  case expr of
    Lit int ->
      contOk (ValueK int : stack)
    Bin op e1 e2 ->
      evalSK e1 (evalSK e2 (contOk . evalOpSK op)) stack
    Throw ->
      failSK stack
    Catch exception handler ->
      evalSK exception (contOk . ignoreHandlerK) (HandlerK (evalSK handler contOk) : stack)

ignoreHandlerK :: [ElementK] -> [ElementK]
ignoreHandlerK (ValueK value : HandlerK handler : stack) =
  ValueK value : stack

failSK :: [ElementK] -> [ElementK]
failSK [] = [] -- where
failSK (ValueK _ : stack) = failSK stack
failSK (HandlerK handlerCont : stack) = handlerCont stack

evalOpSK :: Op -> [ElementK] -> [ElementK]
evalOpSK op (ValueK v2 : ValueK v1 : stack) =
  ValueK (evalOp op v1 v2) : stack
evalOpSK _ _ =
  error "unexpected use of evalOpSK"

data ElementD
  = ValueD Int
  | HandlerD Code

data Code
  = CHalt
  | Push Int Code
  | COp Op Code
  | Fail
  | Ignore Code
  | CCatch Code Code

evalSKD :: Exp -> Code -> Code
{-
evalSKD expr contOk stack =
  case eval expr of
    Nothing ->
      failSK stack
    Just v ->
      contOk (ValueD v : stack)
-}
{-
evalSKD expr contOk stack =
  maybe (failSK stack) (contOk . (: stack) . ValueD) (eval expr)
-}
evalSKD expr contOk =
  case expr of
    Lit int ->
      -- contOk (ValueD int : stack)
      -- contOk . (ValueD int :)
      Push int contOk
    Bin op e1 e2 ->
      -- evalSKD e1 (evalSKD e2 (contOk . evalOpSKD op)) stack
      -- evalSKD e1 (evalSKD e2 (contOk . evalOpSKD op))
      evalSKD e1 (evalSKD e2 (COp op contOk))
    Throw ->
      -- failSKD stack
      -- failSKD
      Fail
    Catch exception handler ->
      -- evalSKD exception (contOk . ignoreHandlerD) (HandlerD (evalSKD handler contOk) : stack)
      -- evalSKD exception (contOk . ignoreHandlerD) . (HandlerD (evalSKD handler contOk) : )
      -- evalSKD exception (Ignore contOk) . (HandlerD (evalSKD handler contOk) : )
      -- evalSKD exception (Ignore contOk) . (HandlerD (evalSKD handler contOk) : )
      CCatch (evalSKD exception (Ignore contOk)) (evalSKD handler contOk)

applyCode :: Code -> [ElementD] -> [ElementD]
applyCode CHalt                      stack = stack
applyCode (Push int contOk)          stack = applyCode contOk (ValueD int : stack)
applyCode (COp op contOk)            stack = applyCode contOk (evalOpSKD op stack)
applyCode Fail                       stack = failSKD stack
applyCode (Ignore contOk)            stack = applyCode contOk (ignoreHandlerD stack)
applyCode (CCatch exception handler) stack = applyCode exception (HandlerD handler : stack)

ignoreHandlerD :: [ElementD] -> [ElementD]
ignoreHandlerD (ValueD value : HandlerD handler : stack) =
  ValueD value : stack

failSKD :: [ElementD] -> [ElementD]
failSKD [] = [] -- where
failSKD (ValueD _ : stack) = failSKD stack
failSKD (HandlerD handlerCont : stack) = applyCode handlerCont stack

evalOpSKD :: Op -> [ElementD] -> [ElementD]
evalOpSKD op (ValueD v2 : ValueD v1 : stack) =
  ValueD (evalOp op v1 v2) : stack
evalOpSKD _ _ =
  error "unexpected use of evalOpSKD"



