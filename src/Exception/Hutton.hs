module Exception.Hutton where

import Prelude hiding (fail)

data Exp
  = Val Int
  | Add Exp Exp
  | Throw
  | Catch Exp Exp
  deriving (Eq, Ord, Show)

data Code
  = Halt
  | Push Int Code
  | CAdd Code
  | Fail
  | Mark Code Code
  | Unmark Code
  deriving (Eq, Ord, Show)

comp :: Exp -> Code
comp expr = comp' expr Halt

comp' :: Exp -> Code -> Code
comp' expr code =
  case expr of
    Val n ->
      Push n code
    Add e1 e2 ->
      comp' e1 (comp' e2 (CAdd code))
    Throw ->
      Fail
    Catch exception handler ->
      Mark (comp' handler code) (comp' exception (Unmark code))

type Stack = [Element]
data Element
  = Value Int
  | Handler Code
  deriving (Eq, Ord, Show)

exec :: Code -> Stack -> Stack
exec Halt stack = stack
exec (Push n code) stack = exec code (Value n : stack)
exec (CAdd code) (Value m : Value n : stack) = exec code (Value (n + m) : stack)
exec Fail stack = fail stack
exec (Mark code' code) stack = exec code (Handler code' : stack)
exec (Unmark code) (Value n : Handler _ : stack) = exec code (Value n : stack)

fail :: Stack -> Stack
fail [] = []
fail (Value n : stack) = fail stack
fail (Handler code : stack) = exec code stack

eval :: Exp -> Stack -> Stack
-- eval expr = exec (comp expr)
-- exec code (eval' expr) = exec (comp' expr code)
eval expr =
  case expr of
    Val n ->
      {- apply specification -}
      -- exec (comp (Val n))
      {- apply `comp` -}
      -- exec (comp' (Val n) Halt)
      {- apply `comp'` -}
      -- exec (Push n Halt)
      {- apply `exec` -}
      -- \stack -> exec Halt (Value n : stack)
      {- apply `exec` -}
      \stack -> Value n : stack
    Add e1 e2 ->
      {- apply specification -}
      -- exec (comp (Add e1 e2))
      {- apply `comp` -}
      -- exec (comp' (Add e1 e2) Halt)
      {- apply `comp'` -}
      exec (comp' e1 (comp' e2 (CAdd Halt)))
      {- apply `exec` -}
      {- apply `exec` -}
    Throw ->
      {- apply specification -}
      exec (comp Throw)
      {- apply `comp` -}
      {- apply `comp'` -}
      {- apply `exec` -}
      {- apply `exec` -}
    Catch exception handler ->
      {- apply specification -}
      exec (comp (Catch exception handler))
      {- apply `comp` -}
      {- apply `comp'` -}
      {- apply `exec` -}
      {- apply `exec` -}

{-
exec (comp' expr code) = eval' expr (exec code) stack
-}
{-
eval' expr (exec code) stack = exec (comp' expr code)
-}
eval' :: Exp -> (Stack -> Stack) -> Stack -> Stack
eval' expr cont stack =
  case expr of
    Val n ->
      cont (Value n : stack)
    Add e1 e2 ->
      undefined


