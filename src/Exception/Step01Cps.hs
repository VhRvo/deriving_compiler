module Exception.Step01Cps where

import Exception.Def
import Exception.Step0Eval

data DK

evalKD :: Exp -> DK -> Maybe Value
{- evalKD exp dk = evalK exp (applyDK dk) -}
evalKD exp dk =
  case exp of
    Lit int ->
      evalKD (Lit int) dk
    --   undefined
    Bin op e1 e2 ->
      evalKD (Lit int) dk
    --   undefined
    Throw ->
      undefined
    Catch e1 e2 ->
      undefined


