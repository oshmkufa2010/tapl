{-# LANGUAGE TupleSections #-}

module Parser (
  parse,
) where

import Text.Parsec hiding (parse)
import Control.Monad.Error.Class
import Data.Char (isSpace)
import qualified Data.List as L

import Types

type Parser a = ParsecT String [Var] (Either String) a

parse :: String -> Either String Term
parse str = do
  result <- runParserT (termP <* (spaces >> eof)) [] "" str
  either (Left . show) Right result

litP :: Parser Term
litP = (string "true" >> return (TmBool True))
  <|> (string "false" >> return (TmBool False))
  <|> (fmap (TmNat . read) (many1 digit))

termP :: Parser Term
termP = try litP <|> varP <|> sExpP

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
sExpP = between (char '(' >> spaces) (spaces >> char ')') $
  (try lambdaP)
  <|> (try letP)
  <|> (try ifP)
  <|> (try fixP)
  <|> (try eqP)
  <|> (try opP)
  <|> applyP
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

    ifP = do
      string "if"
      spaces1 
      cond <- termP
      spaces1
      branch1 <- termP
      spaces1
      branch2 <- termP
      return $ TmIf cond branch1 branch2
    
    fixP = do
      string "fix"
      spaces1
      term <- termP
      spaces
      return $ TmFix term 

    eqP = do
      string "=" <|> string "eq"
      spaces1 
      m <- termP
      spaces1
      n <- termP
      return $ TmEq m n

    opP = do
      op <- (string "+" >> return OpAdd)
        <|> (string "*" >> return OpMult)
        <|> (string "-" >> return OpMinus)
        <|> (string "and" >> return OpAnd)
        <|> (string "or" >> return OpOr)
      spaces1
      m <- termP
      spaces1
      n <- termP
      return $ TmBinOp op m n

    letP = do
      string "let"
      spaces1
      char '[' >> spaces
      var <- idP
      spaces
      char '='
      spaces
      varTerm <- termP
      spaces >> char ']' 
      spaces1
      ctx <- getState
      putState (var : ctx)
      body <- termP 
      putState ctx
      return $ TmLet var varTerm body

    typeHintP :: Parser (Var, Type)
    typeHintP = between (char '[' >> spaces) (spaces >> char ']') $ do
      var <- idP
      between spaces spaces $ char ':'
      types <- primTypeP `sepBy1` (between spaces spaces (string "->"))
      return (var, foldr1 FnType types)
      where
        primTypeP = (string "Bool" >> return BoolType)
          <|> (string "Nat" >> return NatType)
          <|> (string "Unknown" >> return Unknown)
