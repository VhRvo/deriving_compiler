{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exception.EitherFailEvalD where

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
      Right (Value int : stack)
    Bin op e1 e2 ->
      evalS e1 stack >>= evalS e2 >>= Right . evalOpS op
    Throw ->
      Left (fail stack)
    Catch exception handler ->
      handleHandler $
        evalS exception (Handler handler : stack)

fail :: [Element] -> [Element]
fail [] = []
fail (Value _ : stack) = fail stack
fail (Handler handler : stack) = Handler handler : stack

handleHandler :: Either [Element] [Element] -> Either [Element] [Element]
handleHandler (Left (Handler handler : stack)) =
  evalS handler stack
handleHandler (Right (Value value : Handler handler : stack)) =
  Right (Value value : stack)

evalSK :: Exp -> ([Element] -> [Element]) -> ([Element] -> [Element]) -> [Element] -> [Element]
evalSK expr contErr contOk stack =
  case expr of
    Lit int ->
      contOk (Value int : stack)
    Bin op e1 e2 ->
      evalSK e1 contErr (evalSK e2 contErr (contOk . evalOpS op)) stack
    Throw ->
      contErr (fail stack)
    Catch exception handler ->
      evalSK exception (handleHanlderK contErr contOk) (contOk . ignoreHandlerK) (Handler handler : stack)

handleHanlderK :: ([Element] -> [Element]) -> ([Element] -> [Element]) -> [Element] -> [Element]
handleHanlderK contErr contOk (Handler handler : stack) =
  evalSK handler contErr contOk stack

ignoreHandlerK :: [Element] -> [Element]
ignoreHandlerK (Value value : Handler handler : stack) =
  Value value : stack

data Code
  = Halt
  | Push Int Code
  | Ignore Code
  | Op Op Code
  | Fail Code
  | Handle Code Code
  | CHandler Exp Code

evalSKD :: Exp -> Code -> Code -> Code
evalSKD expr contErr contOk =
  case expr of
    Lit int ->
      Push int contOk
    Bin op e1 e2 ->
      evalSKD e1 contErr
        (evalSKD e2 contErr (Op op contOk))
    Throw ->
      Fail contErr
    Catch exception handler ->
      CHandler handler
        (evalSKD exception
          (Handle contErr contOk)
          (Ignore contOk))

-- applyCode :: Code -> [Element] -> [Element]
-- applyCode Halt              stack = stack
-- applyCode (Push int contOk) stack = applyCode contOk (Value int : stack)
-- applyCode (Ignore contOk)   stack = applyCode contOk (Value int : stack)
