module Types (
  Type(..),
  Term(..),
  Var,
  Index,
  BinOp(..),
) where

type Var = String
type Index = Int

data Type =
    BoolType
  | NatType
  | FnType Type Type
  | Unknown
  deriving (Eq)

instance Show Type where
  show BoolType = "Bool"
  show NatType = "Nat"
  show (FnType a@(FnType _ _) b) = "(" ++ (show a) ++ ")" ++ " -> " ++ (show b)
  show (FnType a b) = (show a) ++ " -> " ++ (show b)
  show Unknown = "Unknown"

data BinOp = OpAdd | OpMult | OpMinus | OpAnd | OpOr deriving (Show, Eq)

data Term =
    TmVar Index
  | TmAbs String Type Term 
  | TmApp Term Term
  | TmIf Term Term Term
  | TmBool Bool
  | TmNat Integer
  | TmFix Term
  | TmEq Term Term
  | TmBinOp BinOp Term Term

instance Show Term where
  show (TmVar i) = show i
  show (TmAbs _ _ m) = "λ. " ++ show m
  show (TmApp m@(TmVar _) n@(TmVar _)) = show m ++ " " ++ show n
  show (TmApp m@(TmVar _) n) = show m ++ " (" ++ show n ++ ")"
  show (TmApp m n@(TmVar _)) = "(" ++ show m ++ ") " ++ show n
  show (TmApp m n) = "(" ++ show m ++ ") (" ++ show n ++ ")"
  show (TmIf c b1 b2) = "if (" ++ show c ++ ") then (" ++ show b1 ++ ") else (" ++ show b2 ++ ")"
  show (TmBool b) = show b
  show (TmNat nat ) = show nat
  show (TmFix term) = "Fix (" ++ show term ++ ")"
  show (TmEq m n) = "Eq (" ++ show m ++ " " ++ show n ++ ")"
  show (TmBinOp op m n) = "BinOP (" ++ show op ++ " " ++ show m ++ " " ++ show n ++ ")"
