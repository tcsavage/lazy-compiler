module PrettyPrinter where

import Data.List
import Text.Printf

import AST

pp :: Module -> String
pp (Module name decls) = printf "module %s where\n\n%s" name (unlines $ map ppDecl decls)

ppDecl :: Decl -> String
ppDecl (DTerm ident expr) = printf "%s = %s" (ppIdent ident) (ppExpr expr)
ppDecl (DType ident constrs) = printf "data %s where\n%send" (ppIdent ident) (unlines $ map ppConstr constrs)
    where
        ppConstr ident = printf "    %s;" (ppIdent ident)

ppIdent :: Ident -> String
ppIdent (Id name ty) = printf "%s:%s" name (ppType ty)

ppType :: Type -> String
ppType TyInt = "Int"
ppType (TyFun l r) = printf "(%s -> %s)" (ppType l) (ppType r)
ppType (TyVar ident) = printf "%s" (ppIdent ident)
ppType TyKindStar = "*"

ppExpr :: Expr Ident -> String
ppExpr (L n) = show n
ppExpr (V ident) = ppIdent ident
ppExpr (Lam b e) = printf "\\%s. %s" (ppIdent b) (ppExpr e)
ppExpr (l :@ r) = printf "(%s @ %s)" (ppExpr l) (ppExpr r)
ppExpr (Let rec ds e) = printf "let%s %s in %s" (if rec then "rec" else "") (intercalate ", " $ map ppDef ds) (ppExpr e)
    where
        ppDef (ident, expr) = printf "%s = %s" (ppIdent ident) (ppExpr expr)
ppExpr (Case expr ty alts) = printf "case %s of %s where %s end" (ppExpr expr) (ppType ty) (intercalate "; " $ map ppAlt alts)
    where
        ppAlt (tag, binders, expr) = printf "<%d> %s -> %s" tag (intercalate " " $ map ppBinder binders) (ppExpr expr)
            where
                ppBinder ident = printf "<%s>" (ppIdent ident)
ppExpr (Constr tag ty values) = printf "Pack{%d, %s}{%s}" tag (ppType ty) valuesStr
    where
        valuesStr = intercalate ", " $ map ppExpr values
ppExpr (PrimFun pf) = ppPrimFun pf

ppPrimFun :: PrimFun -> String
ppPrimFun (PrimBinOp op) = ppPrimBinOp op

ppPrimBinOp :: PrimBinOp -> String
ppPrimBinOp PrimAdd = "Add#"
ppPrimBinOp PrimMul = "Mul#"
