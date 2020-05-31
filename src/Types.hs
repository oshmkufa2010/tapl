module Types (
  Type(..),
  Term(..),
  Var,
  Index
) where

type Var = String
type Index = Int

data Type =
    BoolType
  | NatType
  | Unknown
  deriving (Show)

data Term =
    TmVar Index
  | TmAbs String Type Term 
  | TmApp Term Term
  -- | TmIf Term Term Term

instance Show Term where
  show (TmVar i) = show i
  show (TmAbs _ _ m) = "λ. " ++ show m
  show (TmApp m@(TmVar _) n@(TmVar _)) = show m ++ " " ++ show n
  show (TmApp m@(TmVar _) n) = show m ++ " (" ++ show n ++ ")"
  show (TmApp m n@(TmVar _)) = "(" ++ show m ++ ") " ++ show n
  show (TmApp m n) = "(" ++ show m ++ ") (" ++ show n ++ ")"
  -- show (TmIf c b1 b2) = "if (" ++ show c ++ ") then (" ++ show b1 ++ ") else (" ++ show b2 ++ ")"