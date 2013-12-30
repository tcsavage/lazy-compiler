module PrettyPrinter (pp) where

import Data.List
import Text.Printf

import AST

pp :: (Ident, Expr Ident) -> String
pp (ident, exp) = printf "%s = %s" (ppIdent ident) (ppExpr exp)

ppIdent :: Ident -> String
ppIdent (Id name ty) = printf "%s:%s" name (ppType ty)

ppType :: Type -> String
ppType TyInt = "Int"
ppType (TyFun l r) = printf "(%s -> %s)" (ppType l) (ppType r)

ppExpr :: Expr Ident -> String
ppExpr (L n) = show n
ppExpr (V ident) = ppIdent ident
ppExpr (Lam b e) = printf "\\%s. %s" (ppIdent b) (ppExpr e)
ppExpr (l :@ r) = printf "(%s @ %s)" (ppExpr l) (ppExpr r)
ppExpr (Let rec ds e) = printf "let %s in %s" (intercalate ", " $ map pp ds) (ppExpr e)
