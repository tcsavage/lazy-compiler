module STG.PrettyPrint (ppStg) where

import Data.List
import Text.Printf

import STG.AST

ppStg :: Program -> String
ppStg = pp

class PrettyPrint a where
    pp :: a -> String

instance PrettyPrint Program where
    pp (Program bs) = unlines $ map pp bs

instance PrettyPrint Binding where
    pp (Binding var lambda) = printf "%s = %s" var $ pp lambda

instance PrettyPrint Lambda where
    pp (Lambda free update vars expr) = printf "{%s} \\%s {%s} -> %s" (intercalate "," free) (pp update) (intercalate "," vars) (pp expr)

instance PrettyPrint UpdateFlag where
    pp U = "u"
    pp N = "n"

instance PrettyPrint Expr where
    pp (Let bs expr) = printf "let {%s} in %s" (intercalate "; " $ map pp bs) (pp expr)
    pp (LetRec bs expr) = printf "let rec {%s} in %s" (intercalate "; " $ map pp bs) (pp expr)
    pp (Case expr alts) = printf "case %s of { %s }" (pp expr) (pp alts)
    pp (Ap var atoms) = printf "%s {%s}" var (intercalate "," $ map pp atoms)
    pp (Constr tag atoms) = printf "Pack{%d%s}" tag (concat $ map (\a -> ", " ++ pp a) atoms)
    pp (Prim prim atoms) = printf "%s {%s}" (pp prim) (intercalate "," $ map pp atoms)
    pp (Literal lit) = show lit

instance PrettyPrint Prim where
    pp PrimAdd = "AddInt#"
    pp PrimMul = "MulInt#"

instance PrettyPrint Atom where
    pp (AtomVar x) = x
    pp (AtomLit x) = show x

instance PrettyPrint Alts where
    pp (Algebraic aalts def) = intercalate "; " (map pp aalts) ++ "; " ++ pp def
    pp (Primitive palts def) = intercalate "; " (map pp palts) ++ "; " ++ pp def

instance PrettyPrint DefAlt where
    pp (DefBinding name expr) = printf "%s -> %s" name $ pp expr
    pp (Default expr) = printf "DEFAULT -> %s" $ pp expr

instance PrettyPrint AAlt where
    pp (AAlt tag binders expr) = printf "<%d> %s -> %s" tag (intercalate " " binders) $ pp expr

instance PrettyPrint PAlt where
    pp (PAlt lit expr) = printf "%d -> %s" lit $ pp expr
