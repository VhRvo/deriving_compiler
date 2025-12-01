{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use lambda-case" #-}
module Exception.StackingCps where

import Exception.Def
import Exception.Step0Eval
import Exception.Step1Stacking hiding (evalS, evalCatchSHandler, Exception, evalOpS, Value, Element(..), Stack)
import Prelude hiding (fail)


data Element
  = Value Value

type Stack = [Element]

evalSK :: Exp -> (Stack -> Stack) -> Stack -> Stack
evalSK exp cont stack = cont (eval exp : stack)
