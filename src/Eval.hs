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
import Control.Monad (when, unless)

tmMap :: Int -> (Int -> Index -> Term) -> Term -> Term
tmMap _ _ b@(TmBool _) = b
tmMap _ _ n@(TmNat _) = n
tmMap c onVar (TmVar k) = onVar c k
tmMap c onVar (TmAbs var ty tm) = TmAbs var ty $ tmMap (c+1) onVar tm
tmMap c onVar (TmApp m n) = TmApp (tmMap c onVar m) (tmMap c onVar n)
tmMap c onVar (TmIf cond b1 b2) = TmIf (tmMap c onVar cond) (tmMap c onVar b1) (tmMap c onVar b2)
tmMap c onVar (TmFix tm) = TmFix $ tmMap c onVar tm
tmMap c onVar (TmEq m n) = TmEq (tmMap c onVar m) (tmMap c onVar n)
tmMap c onVar (TmBinOp op m n) = TmBinOp op (tmMap c onVar m) (tmMap c onVar n)

tmShift :: Int -> Term -> Term
tmShift d tm = tmMap 0 (\c k -> if k < c then (TmVar k) else (TmVar (k+d))) tm

tmSub :: Index -> Term -> Term -> Term
tmSub j s tm = tmMap 0 (\c k -> if k == j+c then tmShift c s else TmVar k) tm

tmSubTop :: Term -> Term -> Term
tmSubTop s tm = tmShift (-1) $ tmSub 0 (tmShift 1 s) tm

typeOf :: Term -> StateT [Type] (Either String) Type
typeOf (TmNat _) = return NatType
typeOf (TmBool _) = return BoolType
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
    FnType a b | tyN == a -> return b
    _ -> throwError "TmApp: type is not match"
typeOf (TmAbs _ ty m) = do
  env <- get
  put (ty : env)
  tyM <- typeOf m
  put env
  return (FnType ty tyM)
typeOf (TmFix m) = do
  tyM <- typeOf m
  case tyM of
    FnType a b | a == b -> return a
    _ -> throwError "TmFix: type error"
typeOf (TmEq m n) = do
  tyM <- typeOf m
  tyN <- typeOf n
  if tyM == tyN && tyM `elem` [BoolType, NatType]
    then return BoolType
    else throwError "TmEq: type error"
typeOf (TmBinOp op m n) = do
  tyM <- typeOf m
  tyN <- typeOf n
  case op of
    _ | op `elem` [OpAdd, OpMinus, OpMult] && (tyM == NatType && tyN == NatType) -> return NatType
    _ | op `elem` [OpAnd, OpOr] && (tyM == BoolType && tyN == BoolType) -> return BoolType
    _ -> throwError "TmBinOp: type error"

eval :: Term -> StateT [Type] (Either String) Term
eval term = do
  env <- get
  case step env term of
    Just term' -> eval term'
    Nothing -> return term
  where
    step :: [Type] -> Term -> Maybe Term
    step env (TmApp m@(TmAbs _ _ m') n) 
      | isVal env n = return (tmSubTop n m') -- E-APPABS
      | otherwise = fmap (\n' -> TmApp m n') (step env n) -- E-APP2
    step env (TmApp m n) = fmap (\m' -> TmApp m' n) (step env m) -- E-APP1
    step env (TmIf (TmBool True) b1 b2)
      | isVal env b1 = return b1
      | otherwise = step env b1 -- E-IFTRUE
    step env (TmIf (TmBool False) b1 b2)
      | isVal env b2 = return b2
      | otherwise = step env b2 -- E-IFFALSE
    step env (TmIf c b1 b2) = fmap (\c' -> TmIf c' b1 b2) (step env c)
    step _ (TmFix m@(TmAbs _ _ m')) = return $ tmSubTop (TmFix m) m'
    step env (TmFix m) = fmap TmFix (step env m)
    step _ (TmEq (TmBool m) (TmBool n)) = return $ TmBool (m == n)
    step _ (TmEq (TmNat m) (TmNat n)) = return $ TmBool (m == n)
    step env (TmEq m n)
      | isVal env m = fmap (\n' -> TmEq m n') (step env n)
      | otherwise = fmap (\m' -> TmEq m' n) (step env m)
    step env (TmBinOp OpAdd (TmNat m) (TmNat n)) = return $ TmNat (m + n)
    step env (TmBinOp OpMinus (TmNat m) (TmNat n)) = return $ TmNat (m - n)
    step env (TmBinOp OpMult (TmNat m) (TmNat n)) = return $ TmNat (m * n)
    step env (TmBinOp OpAnd (TmBool m) (TmBool n)) = return $ TmBool (m && n)
    step env (TmBinOp OpOr (TmBool m) (TmBool n)) = return $ TmBool (m || n)
    step env (TmBinOp op m n)
      | isVal env m = fmap (\n' -> TmBinOp op m n') (step env n)
      | otherwise = fmap (\m' -> TmBinOp op m' n) (step env m)
    step _ _ = Nothing

    isVal :: [Type] -> Term -> Bool
    isVal _ (TmAbs _ _ _) = True
    isVal _ (TmBool _) = True
    isVal _ (TmNat _) = True
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
    showTerm' (TmNat n) = return (show n)
    showTerm' (TmBool b) = return (show b)
    showTerm' (TmIf c b1 b2) = do
      c' <- showTerm' c
      b1' <- showTerm' b1
      b2' <- showTerm' b2
      return $ "if (" ++ c' ++ ") then (" ++ b1' ++ ") else (" ++ b2' ++ ")"
    showTerm' (TmFix m) = do
      m' <- showTerm' m
      return $ "Fix (" ++ m' ++ ")"
    showTerm' (TmBinOp op m n) = do
      m' <- showTerm' m
      n' <- showTerm' n
      let opStr = case op of
            OpAdd -> "+"
            OpMinus -> "-"
            OpMult -> "*"
            OpAnd -> "and"
            OpOr -> "or"
      return $ "(" ++ m' ++ " " ++ opStr ++ " " ++ n' ++ ")"
