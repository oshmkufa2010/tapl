module Eval (
  tmShift,
  tmSub,
  tmSubTop,
  eval1,
) where

import Types

tmMap :: Int -> (Int -> Index -> Term) -> Term -> Term
tmMap c onVar (TmVar k) = onVar c k
tmMap c onVar (TmAbs var ty tm) = TmAbs var ty $ tmMap (c+1) onVar tm
tmMap c onVar (TmApp m n) = TmApp (tmMap c onVar m) (tmMap c onVar n)
-- tmMap c onVar (TmIf cond b1 b2) = TmIf (tmMap c onVar cond) (tmMap c onVar b1) (tmMap c onVar b2)

tmShift :: Int -> Term -> Term
tmShift d tm = tmMap 0 (\c k -> if k < c then (TmVar k) else (TmVar (k+d))) tm

tmSub :: Index -> Term -> Term -> Term
tmSub j s tm = tmMap 0 (\c k -> if k == j+c then tmShift c s else TmVar k) tm

tmSubTop :: Term -> Term -> Term
tmSubTop s tm = tmShift (-1) $ tmSub 0 (tmShift 1 s) tm

eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs _ _ m) n@(TmAbs _ _ _)) = eval1 $ tmSubTop n m
eval1 (TmApp m@(TmAbs _ _ _) n) = eval1 n >>= \n' -> eval1 $ TmApp m n
eval1 (TmApp m n) = eval1 m >>= \m' -> eval1 $ TmApp m' n
eval1 m@(TmAbs _ _ _) = return m
eval1 _ = Nothing
