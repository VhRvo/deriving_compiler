{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use id" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Arith.Def where

data Op
  = Add
  | Sub
  | Mul
  deriving (Eq, Ord, Show)

data Exp
  = Lit Int
  | Bin Op Exp Exp
  deriving (Eq, Ord, Show)

-- eval' :: Exp -> Int
-- eval' expr = evalK expr id

-- data D1
--   = D1End
--   | D1Op Op Int D1
--   | D1Exp Exp Op D1

-- eval'' :: Exp -> Int
-- eval'' expr = evalKD1 expr D1End

-- applyD1 :: D1 -> Int -> Int
-- applyD1 cont =
--   case cont of
--     D1End ->
--       id
--     D1Op op v1 cont ->
--       \v2 -> (applyD1 cont) (evalOp op v1 v2)
--     D1Exp e2 op cont ->
--       \v1 -> evalKD1 e2 (D1Op op v1 cont)

-- evalKD1 :: Exp -> D1 -> Int
-- {- evalKD1 expr cont = evalK expr (applyD1 cont) -}
-- evalKD1 expr cont =
--   case expr of
--     Lit int ->
--       evalK (Lit int) (applyD1 cont)
--     Bin op e1 e2 ->
--       -- evalK (Bin op e1 e2) (applyD1 cont)
--       {- apply `evalK` -}
--       -- (evalK e1) (\v1 -> (evalK e2) (\v2 -> (applyD1 cont) (evalOp op v1 v2)))
--       {- introducing `D1Op` -}
--       -- (evalK e1) (\v1 -> (evalK e2) (applyD1 (D1Op op v1 cont)))
--       {- unapply `evalKD1` -}
--       -- (evalK e1) (\v1 -> evalKD1 e2 (D1Op op v1 cont))
--       {- introducing local binding  -}
--       {- *** stuck ***,
--          v1 is free variable but bounded by `D1Op`,
--          so I cannot bind `D1Op op v1 cont` locally.
--          The problem is `v1` is captured by closure creation. -}
--       -- (evalK e1) (\v1 -> evalKD1 e2 (D1Op op v1 cont))
--       {- introducing `D1Exp` -}
--       -- (evalK e1) (applyD1 (D1Exp e2 op cont))
--       {- unapply `evalKD1` -}
--       evalKD1 e1 (D1Exp e2 op cont)

-- data D2
--   = D2End
--   | D2Exp Exp Op D2
--   | D2Op Op Int D2

-- applyD2 :: D2 -> Int -> Int
-- applyD2 cont =
--   case cont of
--     D2End ->
--       id
--     D2Exp e2 op cont ->
--       \v1 ->
--         -- (evalK e2) (\v2 -> (applyD2 cont) (evalOp op v1 v2))
--         {- introducing `D2Op` -}
--         -- (evalK e2) (applyD2 (D2Op op v1 cont))
--         {- unapply `evalKD2` -}
--         (evalKD2 e2) (D2Op op v1 cont)
--     D2Op op v1 cont ->
--       \v2 -> (applyD2 cont) (evalOp op v1 v2)

-- evalKD2 :: Exp -> D2 -> Int
-- {- evalKD2 expr cont = evalK expr (applyD2 cont) -}
-- evalKD2 expr cont =
--   case expr of
--     Lit int ->
--       evalK (Lit int) (applyD2 cont)
--     Bin op e1 e2 ->
--       -- evalK (Bin op e1 e2) (applyD2 cont)
--       {- apply `evalK` -}
--       -- (evalK e1) (\v1 -> (evalK e2) (\v2 -> (applyD2 cont) (evalOp op v1 v2)))
--       {- introducing `D2Exp` -}
--       -- (evalK e1) (applyD2 (D2Exp e2 op cont))
--       {- unapply `evalKD2` -}
--       (evalKD2 e1) (D2Exp e2 op cont)

-- data D3
--   = D3End
--   | D3Val Int D3
--   | D3Op Op D3
--   | D3Exp Exp D3

-- applyD3 :: D3 -> Int -> Int
-- applyD3 cont =
--   case cont of
--     D3End ->
--       id
--     D3Val v1 (D3Op op cont) ->
--       \v2 -> (applyD3 cont) (evalOp op v1 v2)
--     D3Exp e2 (D3Op op cont) ->
--       \v1 -> evalKD3 e2 (D3Val v1 (D3Op op cont))
--     _ -> error "applyD3: inconsistent cont"


-- evalKD3 :: Exp -> D3 -> Int
-- {- evalKD3 expr cont = evalK expr (applyD3 cont) -}
-- evalKD3 expr cont =
--   case expr of
--     Lit int ->
--       evalK (Lit int) (applyD3 cont)
--     Bin op e1 e2 ->
--       -- evalK (Bin op e1 e2) (applyD3 cont)
--       {- apply `evalK` -}
--       -- (evalK e1) (\v1 -> (evalK e2) (\v2 -> (applyD3 cont) (evalOp op v1 v2)))
--       {- introducing `D3Val` and `D3Op` -}
--       -- (evalK e1) (\v1 -> (evalK e2) (applyD3 (D3Val v1 (D3Op op cont))))
--       {- unapply `evalKD3` -}
--       -- (evalK e1) (\v1 -> evalKD3 e2 (D3Val v1 (D3Op op cont)))
--       {- introducing local binding  -}
--       {- *** stuck similarly ***,
--          v1 is free variable but bounded by `D3Op`,
--          so I cannot bind `D3Op op v1 cont` locally.
--          The problem is `v1` is captured by closure creation. -}
--       -- (evalK e1) (\v1 -> evalKD3 e2 (D3Val v1 (D3Op op cont)))
--       {- introducing `D3Exp` -}
--       -- (evalK e1) (applyD3 (D3Exp e2 (D3Op op cont)))
--       {- unapply `evalKD3` -}
--       evalKD3 e1 (D3Exp e2 (D3Op op cont))
