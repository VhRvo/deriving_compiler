module Exception.Def where

data Op
  = Add
  | Sub
  | Mul
  deriving (Eq, Ord, Show)

data Exp
  = Lit Int
  | Bin Op Exp Exp
  | Throw
  | Catch Exp Exp
  deriving (Eq, Ord, Show)
