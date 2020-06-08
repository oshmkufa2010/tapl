module Eval (
  tmShift,
  tmSub,
  tmSubTop,
  eval,
  showTerm,
  typeOf,
) where

import Types
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (when)

tmMap :: Int -> (Int -> Index -> Term) -> Term -> Term
tmMap _ _ TmTrue = TmTrue
tmMap _ _ TmFalse = TmFalse
tmMap _ _ TmZero = TmZero
tmMap c onVar (TmVar k) = onVar c k
tmMap c onVar (TmAbs var ty tm) = TmAbs var ty $ tmMap (c+1) onVar tm
tmMap c onVar (TmApp m n) = TmApp (tmMap c onVar m) (tmMap c onVar n)
tmMap c onVar (TmIf cond b1 b2) = TmIf (tmMap c onVar cond) (tmMap c onVar b1) (tmMap c onVar b2)

tmShift :: Int -> Term -> Term
tmShift d tm = tmMap 0 (\c k -> if k < c then (TmVar k) else (TmVar (k+d))) tm

tmSub :: Index -> Term -> Term -> Term
tmSub j s tm = tmMap 0 (\c k -> if k == j+c then tmShift c s else TmVar k) tm

tmSubTop :: Term -> Term -> Term
tmSubTop s tm = tmShift (-1) $ tmSub 0 (tmShift 1 s) tm

typeOf :: Term -> StateT [Type] (Either String) Type
typeOf TmZero = return NatType
typeOf TmTrue = return BoolType
typeOf TmFalse = return BoolType
typeOf (TmIf c b1 b2) = do
  cTy <- typeOf c
  when (cTy /= BoolType) $ throwError "TmIf: type of condition expr is not Bool"
  b1Ty <- typeOf b1
  b2Ty <- typeOf b2
  when (b1Ty /= b2Ty) $ throwError "TmIf: types of branches is different"
  return b1Ty
typeOf (TmVar i) = do
  env <- get
  return (env !! i)
typeOf (TmApp m n) = do
  tyM <- typeOf m
  tyN <- typeOf n
  case tyM of 
    FnType a b -> if tyN == a then return b else throwError "TmApp: type is not match"
    _ -> throwError "TmApp: type is not match"
typeOf (TmAbs _ ty m) = do
  env <- get
  put (ty : env)
  tyM <- typeOf m
  put env
  return (FnType ty tyM)

type Env = [(Var, Type)]

eval :: Term -> StateT Env (Either String) Term
eval term = do
  env <- get
  case step env term of
    Just term' -> eval term'
    Nothing -> return term
  where
    step :: Env -> Term -> Maybe Term
    step env (TmApp m@(TmAbs _ _ m') n) 
      | isVal env n = return (tmSubTop n m') -- E-APPABS
      | otherwise = fmap (\n' -> TmApp m n') (step env n) -- E-APP2
    step env (TmApp m n) = fmap (\m' -> TmApp m' n) (step env m) -- E-APP1
    step env (TmIf TmTrue b1 b2)
      | isVal env b1 = return b1
      | otherwise = step env b1 -- E-IFTRUE
    step env (TmIf TmFalse b1 b2)
      | isVal env b2 = return b2
      | otherwise = step env b2 -- E-IFFALSE
    step env (TmIf c b1 b2) = fmap (\c' -> TmIf c' b1 b2) (step env c)
    step _ _ = Nothing

    isVal :: Env -> Term -> Bool
    isVal _ (TmAbs _ _ _) = True
    isVal _ TmTrue = True
    isVal _ TmFalse = True
    isVal _ TmZero = True
    isVal _ _ = False

showTerm :: Term -> String
showTerm term = evalState (showTerm' term) []
  where
    showTerm' :: Term -> State [String] String
    showTerm' (TmAbs name ty m) = do
      env <- get
      let name' = if name `elem` env then (name ++ "'") else name
      put (name' : env)
      next <- showTerm' m
      put env
      return $ "λ" ++ name' ++ ":" ++ (show ty) ++ "." ++ next
    showTerm' (TmApp m n) = do
      term1 <- showTerm' m
      term2 <- showTerm' n
      return $ case (m, n) of
        (TmVar _, TmVar _) -> term1 ++ " " ++ term2
        (TmVar _, _) -> term1 ++ " (" ++ term2 ++ ")"
        (_, TmVar _) -> "(" ++ term1 ++ ") " ++ term2
        _ -> "(" ++ term1 ++ ") " ++ "(" ++ term2 ++ ")"
    showTerm' (TmVar i) = do
      env <- get
      return $ env !! i 
    showTerm' TmZero = return "0"
    showTerm' TmTrue = return "true"
    showTerm' TmFalse = return "false"
    showTerm' (TmIf c b1 b2) = do
      c' <- showTerm' c
      b1' <- showTerm' b1
      b2' <- showTerm' b2
      return $ "if (" ++ c' ++ ") then (" ++ b1' ++ ") else (" ++ b2' ++ ")"
