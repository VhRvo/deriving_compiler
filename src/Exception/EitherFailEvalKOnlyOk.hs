{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exception.EitherFailEvalKOnlyOk where

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
      -- evalSK (Lit int) contOk stack
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) (eval (Lit int))
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) (Just int)
      contOk (ValueK int : stack)
    Bin op e1 e2 ->
      -- evalSK (Bin op e1 e2) contOk stack
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) (eval (Bin op e1 e2))
      {- -}
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) $ do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2)
      {- -}
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) $
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
      --     -- maybe (failSK stack) (contOk . (: stack) . ValueK) Nothing
      --     failSK stack
      --   Just v1 ->
      --     -- case eval e2 of
      --     --   Nothing ->
      --     --     -- maybe (failSK stack) (contOk . (: stack) . ValueK) Nothing
      --     --     -- failSK stack
      --     --     {- definition of `failSK` -}
      --     --     failSK (ValueK v1 : stack)
      --     --   Just v2 ->
      --     --     -- maybe (failSK stack) (contOk . (: stack) . ValueK) $ Just (evalOp op v1 v2)
      --     --     -- contOk (ValueK (evalOp op v1 v2) : stack)
      --     --     -- contOk (evalOpSK op (ValueK v2 : ValueK v1 : stack))
      --     --     contOk . evalOpSK op . (: (ValueK v1 : stack)) . ValueK $ v2
      --     {- -}
      --     -- maybe (failSK (ValueK v1 : stack)) (contOk . evalOpSK op . (: (ValueK v1 : stack)) . ValueK) (eval e2)
      --     {- -}
      --     -- maybe (failSK (ValueK v1 : stack)) (contOk . evalOpSK op . (: (ValueK v1 : stack)) . ValueK) (eval e2)
      --     {- -}
      --     evalSK e2 (contOk . evalOpSK op) (ValueK v1 : stack)
      {- -}
      -- maybe (failSK stack) (evalSK e2 (contOk . evalOpSK op) . (: stack) . ValueK) (eval e1)
      {- -}
      evalSK e1 (evalSK e2 (contOk . evalOpSK op)) stack
    Throw ->
      -- evalSK Throw contOk stack
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) (eval Throw)
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) Nothing
      failSK stack
    Catch exception handler ->
      -- evalSK (Catch exception handler) contOk stack
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) (eval (Catch exception handler))
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) (eval exception <|> eval handler)
      {- -}
      -- maybe (failSK stack) (contOk . (: stack) . ValueK) $
      --   case eval exception of
      --     Nothing ->
      --       eval handler
      --     Just value ->
      --       Just value
      {- -}
      -- case eval exception of
      --   Nothing ->
      --     -- maybe (failSK stack) (contOk . (: stack) . ValueK) $ eval handler
      --     -- evalSK handler contOk stack
      --     failSK (HandlerK (evalSK handler contOk) : stack)

      --     -- failSK (HandlerK handler contOk : stack)
      --     -- handleHandlerK contOk (HandlerK handler : stack)
      --     -- handleHandlerK contOk (failSK (HandlerK handler : stack))
      --   Just value ->
      --     -- maybe (failSK stack) (contOk . (: stack) . ValueK) $ Just value
      --     -- contOk (ValueK value : stack)
      --     contOk (ignoreHandlerK (ValueK value : HandlerK (evalSK handler contOk) : stack))

      {- -}
      -- case eval exception of
      --   Nothing ->
      --     failSK (HandlerK (evalSK handler contOk) : stack)
      --   Just value ->
      --     (contOk . ignoreHandlerK) (ValueK value : HandlerK (evalSK handler contOk) : stack)
      {- -}
      -- maybe
      --   (failSK (HandlerK (evalSK handler contOk) : stack))
      --   ((contOk . ignoreHandlerK) . (: (HandlerK (evalSK handler contOk) : stack)) . ValueK)
      --   (eval exception)
      evalSK exception (contOk . ignoreHandlerK) (HandlerK (evalSK handler contOk) : stack)

      {- error calculating -}
      -- handleHanlderK contOk $
      --   case eval exception of
      --     Nothing ->
      --       failSK (HandlerK handler : stack)
      --     Just value ->
      --       ValueK value : HandlerK handler : stack
      {- -}
      -- handleHanlderK contOk $
      --   case eval exception of
      --     Nothing ->
      --       failSK (HandlerK handler : stack)
      --     Just value ->
      --       ValueK value : HandlerK handler : stack


      -- evalSK exception (handleHandlerK contOk) (HandlerK handler : stack)

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
