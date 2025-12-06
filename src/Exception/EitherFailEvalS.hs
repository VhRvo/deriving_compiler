{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exception.EitherFailEvalS where

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
      -- maybe (Left (fail stack)) (Right . (: stack) . Value) $
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
      evalS e1 stack >>= evalS e2 >>= Right . evalOpS op
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
fail (Value _ : stack) = fail stack
fail (Handler handler : stack) = Handler handler : stack

handleHandler :: Either [Element] [Element] -> Either [Element] [Element]
handleHandler (Left (Handler handler : stack)) =
  evalS handler stack
handleHandler (Right (Value value : Handler handler : stack)) =
  Right (Value value : stack)

{- garbage functions
failException :: [Element] -> Either [Element] [Element]
failException (Handler handler : stack') = evalS handler stack'

ignoreHandler :: [Element] -> [Element]
ignoreHandler (Value value : Handler _ : stack) = Value value : stack

errBind :: Either e a -> (e -> Either c a) -> Either c a
errBind (Left err) f = f err
errBind (Right value) _ = Right value
-}


{-
evalSK :: Exp -> ([Element] -> [Element]) -> ([Element] -> [Element]) -> [Element] -> [Element]
{- prehaps wrong/very partial specification, doesn't use the inner structure of `evalS` which is defined in terms of `eval`
evalSK expr contErr contOk stack =
  case evalS expr stack of
    Left errStack ->
      contErr errStack
    Right successStack ->
      contOk successStack
-}
{-
evalSK expr contErr contOk stack =
  either contErr contOk (evalS expr stack)
-}
evalSK expr contErr contOk stack =
  case expr of
    Lit int ->
      -- evalSK (Lit int) contErr contOk stack
      -- either contErr contOk (evalS (Lit int) stack)
      -- either contErr contOk (Right (Value int : stack))
      contOk (Value int : stack)
    Bin op e1 e2 ->
      -- evalSK (Bin op e1 e2) contErr contOk stack
      -- either contErr contOk (evalS (Bin op e1 e2) stack)
      {- -}
      -- either contErr contOk $
      --   evalS e1 stack >>= evalS e2 >>= Right . evalOpS op
      {- -}
      -- either contErr contOk $
      --   case evalS e1 stack of
      --     Left errStack1 ->
      --       Left errStack1
      --     Right successStack1 ->
      --       case evalS e2 successStack1 of
      --         Left errStack2 ->
      --           Left errStack2
      --         Right successStack2 ->
      --           Right (evalOpS op successStack2)
      {- -}
      -- case evalS e1 stack of
      --   Left errStack1 ->
      --     -- either contErr contOk $
      --     --   Left errStack1
      --     contErr errStack1
      --   Right successStack1 ->
      --     -- case evalS e2 successStack1 of
      --     --   Left errStack2 ->
      --     --     -- either contErr contOk $
      --     --     --   Left errStack2
      --     --     contErr errStack2
      --     --   Right successStack2 ->
      --     --     -- either contErr contOk $
      --     --     --   Right (evalOpS op successStack2)
      --     --     {- -}
      --     --     -- contOk (evalOpS op successStack2)
      --     --     {- -}
      --     --     (contOk . evalOpS op) successStack2
      --     -- either contErr (contOk . evalOpS op) (evalS e2 successStack1)
      --     -- evalSK e2 contErr (contOk . evalOpS op) successStack1
      --     (evalSK e2 contErr (contOk . evalOpS op)) successStack1
      {- -}
      -- either contErr (evalSK e2 contErr (contOk . evalOpS op)) (evalS e1 stack)
      {- -}
      -- either contErr (evalSK e2 contErr (contOk . evalOpS op)) (evalS e1 stack)
      {- -}
      evalSK e1 contErr (evalSK e2 contErr (contOk . evalOpS op)) stack
    Throw ->
      -- evalSK Throw contErr contOk stack
      {- -}
      -- either contErr contOk (evalS Throw stack)
      {- -}
      -- either contErr contOk (Left stack)
      {- -}
      contErr stack
    Catch exception handler ->
      -- evalSK (Catch exception handler) contErr contOk stack
      {- -}
      -- either contErr contOk (evalS (Catch exception handler) stack)
      {- -}
      -- either contErr contOk $
      --   handleHandler $
      --     evalS exception (Handler handler : stack)
      {- -}
      -- either contErr contOk $
      --   handleHandler $
      --     case evalS exception (Handler handler : stack) of
      --       Left (Handler handler : stack) ->
      --         Left (Handler handler : stack)
      --       Right (Value value : Handler handler : stack) ->
      --         Right (Value value : Handler handler : stack)
      {- -}
      -- either contErr contOk $
      --   case evalS exception (Handler handler : stack) of
      --     Left (Handler handler : stack) ->
      --       -- handleHandler $
      --       --   Left (Handler handler : stack)
      --       {- -}
      --       evalS handler stack
      --     Right (Value value : Handler handler : stack) ->
      --       -- handleHandler $
      --       --   Right (Value value : Handler handler : stack)
      --       {- -}
      --       Right (Value value : stack)
      {- -}
      -- case evalS exception (Handler handler : stack) of
      --   Left (Handler handler : stack) ->
      --     -- either contErr contOk $
      --     --   evalS handler stack
      --     {- -}
      --     -- evalSK handler contErr contOk stack
      --     {- -}
      --     handleHandlerK contErr contOk (Handler handler : stack)
      --   Right (Value value : Handler handler : stack) ->
      --     -- either contErr contOk $
      --     --   Right (Value value : stack)
      --     {- -}
      --     -- contOk (Value value : stack)
      --     -- handleHandlerK contErr contOk (Value value : Handler handler : stack)
      --     ignoreHandlerK contOk (Value value : Handler handler : stack)
      {- -}
      -- case evalS exception (Handler handler : stack) of
      --   Left (Handler handler : stack) ->
      --     handleHandlerK contErr contOk (Handler handler : stack)
      --   Right (Value value : Handler handler : stack) ->
      --     ignoreHandlerK contOk (Value value : Handler handler : stack)
      {- -}
      -- either (handleHandlerK contErr contOk) (ignoreHandlerK contOk) $
      --   evalS exception (Handler handler : stack)
      {- -}
      evalSK exception (handleHandlerK contErr contOk) (ignoreHandlerK contOk) $
        (Handler handler : stack)

-- handleHandlerK :: ([Element] -> [Element]) -> ([Element] -> [Element]) -> [Element] -> [Element]
-- -- handleHandlerK contErr contOk (Value value : Handler handler : stack) =
-- --   contOk (Value value : stack)
-- handleHandlerK contErr contOk (Handler handler : stack) =
--   evalSK handler contErr contOk stack

handleHandlerK :: ([Element] -> [Element]) -> ([Element] -> [Element]) -> [Element] -> [Element]
handleHandlerK contErr contOk (Handler handler : stack) =
  evalSK handler contErr contOk stack

ignoreHandlerK :: ([Element] -> [Element]) -> [Element] -> [Element]
ignoreHandlerK contOk (Value value : Handler _ : stack) =
  contOk (Value value : stack)


      -- either contErr (contOk . ignoreHandler) $
      --   (`errBind` failException) $
      --     evalS exception (Handler handler : stack)
      {- -}
      -- either contErr (contOk . ignoreHandler) $
      --   (`errBind` failException) $
      --     case evalS exception (Handler handler : stack) of
      --       Left errStack ->
      --         Left errStack
      --       Right successStack ->
      --         Right successStack
      {- -}
      -- either contErr (contOk . ignoreHandler) $
      --   case evalS exception (Handler handler : stack) of
      --     Left errStack ->
      --       -- (`errBind` failException) $
      --       --   Left errStack
      --       failException errStack
      --     Right successStack ->
      --       -- (`errBind` failException) $
      --       --   Right successStack
      --       Right successStack
      {- -}
      -- case evalS exception (Handler handler : stack) of
      --   -- Left errStack ->
      --   --   either contErr (contOk . ignoreHandler) $
      --   --     failException errStack
      --   {- -}
      --   Left (Handler handler : stack') ->
      --     -- either contErr (contOk . ignoreHandler) $
      --     --   evalS handler stack'
      --     {- -}
      --     evalSK handler contErr (contOk . ignoreHandler) stack'
      --   -- Right successStack ->
      --   --   (contOk . ignoreHandler) $ successStack
      --   Right (Value value : Handler handler : stack') ->
      --     -- (contOk . ignoreHandler) $ (Value value : Handler handler : stack)
      --     {- -}
      --     contOk $ (Value value : stack')
      {- -}
      -- case evalS exception (Handler handler : stack) of
      --   Left errStack ->
      --     -- either contErr (contOk . ignoreHandler) $
      --     --   (`errBind` failException) $
      --     --     Left errStack
      --     {- -}
      --     either contErr (contOk . ignoreHandler) $
      --       failException errStack
      --       (`errBind` failException) $
      --         Left errStack
      --   Right successStack ->
      --     either contErr (contOk . ignoreHandler) $
      --       (`errBind` failException) $
      --         Right successStack
    {- -}
    {- -}
    {- -}
-}

evalSK :: Exp -> ([Element] -> [Element]) -> ([Element] -> [Element]) -> [Element] -> [Element]
{- prehaps wrong/very partial specification, doesn't use the inner structure of `evalS` which is defined in terms of `eval`
evalSK expr contErr contOk stack =
  case evalS expr stack of
    Left failedStack ->
      contErr failedStack
    Right successStack ->
      contOk successStack
-}
{-
evalSK expr contErr contOk stack =
  case eval expr of
    Nothing ->
      contErr (fail stack)
    Just v ->
      contOk (Value v : stack)
-}
{-
evalSK expr contErr contOk stack =
  maybe (contErr (fail stack)) (contOk . (: stack) . Value) (eval expr)
-}
evalSK expr contErr contOk stack =
  case expr of
    Lit int ->
      -- evalSK (Lit int) contErr contOk stack
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) (eval (Lit int))
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) (Just int)
      contOk (Value int : stack)
    Bin op e1 e2 ->
      -- evalSK (Bin op e1 e2) contErr contOk stack
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) (eval (Bin op e1 e2))
      {- -}
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) $ do
      --   v1 <- eval e1
      --   v2 <- eval e2
      --   pure (evalOp op v1 v2)
      {- -}
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) $
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
      --     -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) Nothing
      --     contErr (fail stack)
      --   Just v1 ->
      --     -- case eval e2 of
      --     --   Nothing ->
      --     --     -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) Nothing
      --     --     -- contErr (fail stack)
      --     --     {- definition of `fail` -}
      --     --     contErr (fail (Value v1 : stack))
      --     --   Just v2 ->
      --     --     -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) $ Just (evalOp op v1 v2)
      --     --     -- contOk (Value (evalOp op v1 v2) : stack)
      --     --     -- contOk (evalOpS op (Value v2 : Value v1 : stack))
      --     --     contOk . evalOpS op . (: (Value v1 : stack)) . Value $ v2
      --     {- -}
      --     -- maybe (contErr (fail (Value v1 : stack))) (contOk . evalOpS op . (: (Value v1 : stack)) . Value) (eval e2)
      --     {- -}
      --     -- maybe (contErr (fail (Value v1 : stack))) (contOk . evalOpS op . (: (Value v1 : stack)) . Value) (eval e2)
      --     {- -}
      --     evalSK e2 contErr (contOk . evalOpS op) (Value v1 : stack)
      {- -}
      -- maybe (contErr (fail stack)) (evalSK e2 contErr (contOk . evalOpS op) . (: stack) . Value) (eval e1)
      {- -}
      evalSK e1 contErr (evalSK e2 contErr (contOk . evalOpS op)) stack
    Throw ->
      -- evalSK Throw contErr contOk stack
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) (eval Throw)
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) Nothing
      contErr (fail stack)
    Catch exception handler ->
      -- evalSK (Catch exception handler) contErr contOk stack
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) (eval (Catch exception handler))
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) (eval exception <|> eval handler)
      {- -}
      -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) $
      --   case eval exception of
      --     Nothing ->
      --       eval handler
      --     Just value ->
      --       Just value
      {- -}
      -- case eval exception of
      --   Nothing ->
      --     -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) $ eval handler
      --     -- evalSK handler contErr contOk stack
      --     -- handleHanlderK contErr contOk (Handler handler : stack)
      --     handleHanlderK contErr contOk (fail (Handler handler : stack))
      --   Just value ->
      --     -- maybe (contErr (fail stack)) (contOk . (: stack) . Value) $ Just value
      --     -- contOk (Value value : stack)
      --     contOk . ignoreHandlerK $ (Value value : Handler handler : stack)
      {- -}
      -- maybe
      --   (handleHanlderK contErr contOk (fail (Handler handler : stack)))
      --   (contOk . ignoreHandlerK . (: (Handler handler : stack)) . Value)
      --   (eval exception)
      {- -}
      evalSK exception (handleHanlderK contErr contOk) (contOk . ignoreHandlerK) (Handler handler : stack)

handleHanlderK :: ([Element] -> [Element]) -> ([Element] -> [Element]) -> [Element] -> [Element]
handleHanlderK contErr contOk (Handler handler : stack) =
  evalSK handler contErr contOk stack

ignoreHandlerK :: [Element] -> [Element]
ignoreHandlerK (Value value : Handler handler : stack) =
  Value value : stack

{-
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
-}
