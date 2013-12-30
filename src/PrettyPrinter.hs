module PrettyPrinter (pp) where

import Data.List
import Text.Printf

import AST

pp :: Module -> String
pp (Module name decls) = printf "module %s where\n\n%s" name (unlines $ map ppDecl decls)

ppDecl :: (Ident, Expr Ident) -> String
ppDecl (ident, exp) = printf "%s = %s" (ppIdent ident) (ppExpr exp)

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
ppExpr (Let rec ds e) = printf "let%s %s in %s" (if rec then "rec" else "") (intercalate ", " $ map ppDecl ds) (ppExpr e)
