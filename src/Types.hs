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
  | TmLet Var Term Term
  deriving (Show, Eq)
