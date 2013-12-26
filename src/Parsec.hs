module Parsec where

import Control.Applicative (pure)
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

import AST

parseSource :: String -> Either ParseError Decl
parseSource input = parse decl "(unknown)" input

file :: GenParser Char st [Decl]
file = do
    decls <- many decl
    eof
    pure decls

decl :: GenParser Char st Decl
decl = do
    n <- parseIdent
    spaces
    char '='
    spaces
    e <- parseExpr
    pure $ Decl n e

parseIdent :: GenParser Char st Ident
parseIdent = do
    n <- parseName
    spaces
    char ':'
    spaces
    ty <- parseType
    pure $ Id n ty

parseName :: GenParser Char st String
parseName = do
    c0 <- letter
    rest <- many alphaNum
    pure $ c0:rest

parseType :: GenParser Char st Type
parseType = try parseType_TyFun <|> try parseType_Parens <|> try parseType_TyInt

parseType_Parens :: GenParser Char st Type
parseType_Parens = do
    char '('
    ty <- parseType
    char ')'
    pure $ ty

parseType_TyInt :: GenParser Char st Type
parseType_TyInt = do
    parseName
    pure TyInt

parseType_TyFun :: GenParser Char st Type
parseType_TyFun = do
    l <- try parseType_Parens <|> try parseType_TyInt
    spaces
    string "->"
    spaces
    r <- parseType
    pure $ TyFun l r

parseExpr :: GenParser Char st (Expr Ident)
parseExpr = do
    es <- sepBy1 (try parseExpr_Parens <|> try parseExpr_Lam <|> try parseExpr_Var <|> try parseExpr_Lit) (spaces >> char '@' >> spaces)
    pure $ foldl1 (:@) es

parseExpr_Parens :: GenParser Char st (Expr Ident)
parseExpr_Parens = do
    char '('
    e <- parseExpr
    char ')'
    pure e

parseExpr_Lit :: GenParser Char st (Expr Ident)
parseExpr_Lit = do
    n <- many digit
    pure $ L $ read n

parseExpr_Var :: GenParser Char st (Expr Ident)
parseExpr_Var = fmap V parseIdent

parseExpr_Lam :: GenParser Char st (Expr Ident)
parseExpr_Lam = do
    char '\\'
    spaces
    binder <- parseIdent
    spaces
    char '.'
    spaces
    e <- parseExpr
    pure $ Lam binder e

--parseExpr_Ap :: GenParser Char st (Expr Ident -> Expr Ident -> Expr Ident)
--parseExpr_Ap = do
--    char '@'
