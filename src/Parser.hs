{-# LANGUAGE TupleSections #-}

module Parser (
  Type(..),
  Term(..),
  parse,
) where

import Text.Parsec hiding (parse)
import Control.Monad.Error.Class
import Data.Char (isSpace)
import qualified Data.List as L

import Types

type Parser a = ParsecT String [Var] (Either String) a

parse :: String -> [Var] -> Either String Term
parse str ctx = do
  result <- runParserT (termP <* eof) ctx "" str
  either (Left . show) Right result

termP :: Parser Term
termP = varP <|> sExpP

spaces1 :: Parser ()
spaces1 = skipMany1 space

idP :: Parser String
idP = many1 $ satisfy (\c -> not (isSpace c) && c `notElem` "()[]:")

varP :: Parser Term
varP = do
  var <- idP
  ctx <- getState
  case L.findIndex (== var) ctx of
    Just i -> return $ TmVar i
    Nothing -> throwError $ var ++ " is not bound"

sExpP :: Parser Term
sExpP = between (char '(' >> spaces) (spaces >> char ')') $ lambdaP <|> applyP
  where
    lambdaP = do
      string "lambda" <|> string "λ"
      (var, ty) <- between spaces1 spaces1 $ fmap (,Unknown) idP <|> typeHintP
      ctx <- getState
      putState $ var : ctx 
      term <- termP
      spaces
      putState ctx
      return $ TmAbs var ty term

    applyP = do
      m <- termP
      spaces1
      n <- termP
      return $ TmApp m n

    -- ifP = do
    --   string "if"
    --   spaces1 
    --   cond <- termP
    --   spaces1
    --   branch1 <- termP
    --   spaces1
    --   branch2 <- termP
    --   return $ TmIf cond branch1 branch2


typeHintP :: Parser (Var, Type)
typeHintP = between (char '[' >> spaces) (spaces >> char ']') $ do
  var <- idP
  between spaces spaces $ char ':'
  ty <- (string "Boolean" >> return BoolType)
    <|> (string "Nat" >> return NatType)
    <|> (string "Unknown" >> return Unknown)
  return (var, ty)

-- (lambda f ((lambda x (f (lambda y ((x x) y)))) (lambda x (f (lambda y ((x x) y))))))
-- (lambda z ((lambda y (lambda x x)) (lambda x (z x))))
-- (lambda n (lambda s (lambda z (s ((n s) z)))))