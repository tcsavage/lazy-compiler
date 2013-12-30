module Parsec where

import Control.Applicative (pure, (<*>), (*>), (<$>))
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Token

import AST

parseSource :: String -> Either ParseError Decl
parseSource input = parse parseDecl "(unknown)" input

lang :: LanguageDef st
lang = LanguageDef { commentStart = "{-"
                   , commentEnd = "-}"
                   , commentLine = "--"
                   , nestedComments = True
                   , identStart = letter <|> char '_'
                   , identLetter = alphaNum <|> char '_'
                   , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                   , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                   , reservedNames = ["let, in", "module", "interface", "implements", "where"]
                   , reservedOpNames = ["@", ":", "\\", ".", "->", "="]
                   , caseSensitive = True
                   }

tokParse :: TokenParser st
tokParse = makeTokenParser lang

parseDecl :: Parsec String st Decl
parseDecl = do
    ident <- parseIdent
    reservedOp tokParse "="
    expr <- parseExpr
    pure $ Decl ident expr

parseIdent :: Parsec String st Ident
parseIdent = do
    name <- identifier tokParse
    reservedOp tokParse ":"
    ty <- parseType
    pure $ Id name ty

parseType :: Parsec String st Type
parseType = chainr1 (parens tokParse parseType <|> parseType_Int) (reservedOp tokParse "->" >> pure TyFun)

parseType_Int :: Parsec String st Type
parseType_Int = do
    tyIdent <- identifier tokParse
    pure TyInt

parseExpr :: Parsec String st (Expr Ident)
parseExpr = parseExpr_Let <|> parseExpr_Aps

parseExpr_Aps :: Parsec String st (Expr Ident)
parseExpr_Aps = chainl1 (parens tokParse parseExpr <|> parseExpr_Lam <|> parseExpr_Var <|> parseExpr_Lit) (reservedOp tokParse "@" >> pure (:@))

parseExpr_Let :: Parsec String st (Expr Ident)
parseExpr_Let = do
    reserved tokParse "let"
    defs <- commaSep tokParse (unDecl <$> parseDecl)
    reserved tokParse "in"
    expr <- parseExpr_Aps
    pure $ Let True defs expr

parseExpr_Lam :: Parsec String st (Expr Ident)
parseExpr_Lam = do
    reservedOp tokParse "\\"
    ident <- parseIdent
    reservedOp tokParse "."
    expr <- parseExpr
    pure $ Lam ident expr

parseExpr_Var :: Parsec String st (Expr Ident)
parseExpr_Var = V <$> parseIdent

parseExpr_Lit :: Parsec String st (Expr Ident)
parseExpr_Lit = (L . fromInteger) <$> integer tokParse
