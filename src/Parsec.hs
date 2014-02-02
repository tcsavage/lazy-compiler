module Parsec where

import Control.Applicative (pure, (<*>), (*>), (<$>))
import Control.Monad
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Token
import Text.Printf

import AST

-- | Parse a global definition.
parseSource :: String -> Either ParseError Module
parseSource input = parse parseModule "(unknown)" input

-- | Core language definition.
lang :: LanguageDef st
lang = LanguageDef { commentStart = "{-"
                   , commentEnd = "-}"
                   , commentLine = "--"
                   , nestedComments = True
                   , identStart = letter <|> char '_'
                   , identLetter = alphaNum <|> char '_'
                   , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                   , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                   , reservedNames = ["let", "letrec", "in", "module", "interface", "implements", "data", "case", "of", "where", "end", "Int", "Add#", "Mul#"]
                   , reservedOpNames = ["@", ":", "\\", ".", "->", "=", ";"]
                   , caseSensitive = True
                   }

tokParse :: TokenParser st
tokParse = makeTokenParser lang

parseModule :: Parsec String st Module
parseModule = do
    reserved tokParse "module"
    modName <- identifier tokParse
    reserved tokParse "where"
    decls <- manyTill parseDecl eof
    pure $ Module modName decls

parseDecl :: Parsec String st Decl
parseDecl = try parseDecl_Type <|> parseDecl_Term

parseDecl_Type :: Parsec String st Decl
parseDecl_Type = do
    reserved tokParse "data"
    typeName <- identifier tokParse
    reserved tokParse "where"
    constrs <- sepBy parseIdent (reservedOp tokParse ";")
    reserved tokParse "end"
    pure $ DType (Id typeName TyKindStar) constrs

parseDecl_Term :: Parsec String st Decl
parseDecl_Term = do
    ident <- parseIdent
    reservedOp tokParse "="
    expr <- parseExpr
    pure $ DTerm ident expr

parseIdent :: Parsec String st Ident
parseIdent = do
    name <- identifier tokParse
    reservedOp tokParse ":"
    ty <- parseType
    pure $ Id name ty

parseType :: Parsec String st Type
parseType = chainr1 (parens tokParse parseType <|> parseType_Int <|> parseType_Var) (reservedOp tokParse "->" >> pure TyFun)

parseType_Int :: Parsec String st Type
parseType_Int = do
    reserved tokParse "Int"
    pure TyInt

parseType_Var :: Parsec String st Type
parseType_Var = do
    name <- identifier tokParse
    pure $ TyVar $ Id name TyKindStar

parseExpr :: Parsec String st (Expr Ident)
parseExpr = parseExpr_Let <|> parseExpr_Aps

parseExpr_Aps :: Parsec String st (Expr Ident)
parseExpr_Aps = chainl1 (parens tokParse parseExpr <|> parseExpr_Lam <|> parseExpr_Case <|> parseExpr_PrimOp <|> parseExpr_Constr <|> parseExpr_Var <|> parseExpr_Lit) (reservedOp tokParse "@" >> pure (:@))

parseExpr_Let :: Parsec String st (Expr Ident)
parseExpr_Let = do
    rec <- try (reserved tokParse "letrec" >> pure True) <|> try (reserved tokParse "let" >> pure False)
    defs <- commaSep tokParse ((\(DTerm i e) -> (i, e)) <$> parseDecl_Term)
    reserved tokParse "in"
    expr <- parseExpr_Aps
    pure $ Let rec defs expr

parseExpr_Constr :: Parsec String st (Expr Ident)
parseExpr_Constr = do
    symbol tokParse "Pack{"
    tag <- fromInteger `fmap` natural tokParse
    symbol tokParse ","
    ty <- parseType
    symbol tokParse "}"
    symbol tokParse "{"
    values <- sepBy parseExpr (symbol tokParse ",")
    symbol tokParse "}"
    pure $ Constr tag ty values

parseExpr_Case :: Parsec String st (Expr Ident)
parseExpr_Case = do
    reserved tokParse "case"
    expr <- parseExpr
    reserved tokParse "of"
    ty <- parseType
    reserved tokParse "where"
    alts <- sepEndBy parseCaseAlt (symbol tokParse ";")
    reserved tokParse "end"
    pure $ Case expr ty alts

parseExpr_PrimOp :: Parsec String st (Expr Ident)
parseExpr_PrimOp = parseOpName <$> choice primOpNames
  where
    primOpNames = map (lexeme tokParse . string) ["Add#", "Mul#"]
    parseOpName "Add#" = PrimFun $ PrimBinOp PrimAdd
    parseOpName "Mul#" = PrimFun $ PrimBinOp PrimMul
    parseOpName n = error $ printf "No parse def for primop `%n`." n

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

parseCaseAlt :: Parsec String st (Alt Ident)
parseCaseAlt = do
    tag <- fromInteger <$> (angles tokParse $ natural tokParse)
    binders <- manyTill (angles tokParse parseIdent) (symbol tokParse "->")
    expr <- parseExpr
    pure (tag, binders, expr)
