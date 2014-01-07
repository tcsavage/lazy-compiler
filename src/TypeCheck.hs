{-# LANGUAGE PackageImports #-}

module TypeCheck where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe
import Text.Printf

import AST
import PrettyPrinter

checkModule :: Module -> Module
checkModule m@(Module _ decls) = if runReader (and <$> mapM typecheck decls) $ buildTypeMap decls then m else m

typecheck :: Decl -> Reader (M.Map String Type) Bool
typecheck (DTerm (Id name ty) expr) = do
    ty2 <- getType expr
    case ty == ty2 of
        True -> pure True
        False -> error $ printf "Type mismatch. Decl `%s` has type %s but its definition has type %s." name (ppType ty) (ppType ty2)
typecheck (DType ident@(Id name kind) constrs) = do
    case all (\(Id name ty) -> returnType ty == (TyVar ident)) constrs of
        True -> pure True
        False -> error $ printf "Data constructor must return type `%s`" name

getType :: Expr Ident -> Reader (M.Map String Type) Type
getType (L _) = pure TyInt
getType (Lam ident expr) = do
    eTy <- local (insertIdent ident) $ getType expr
    pure $ typeOf ident ~> eTy
getType (V ident) = do
    env <- M.lookup (name ident) <$> ask
    case env of
        Nothing -> error $ printf "Identifier %s not in scope." (name ident)
        Just ty -> case ty == typeOf ident of
            True -> pure ty
            False -> error $ printf "Type mismatch. Identifier %s has type %s but value in scope has type %s." (name ident) (ppType $ typeOf ident) (ppType ty)
getType (l :@ r) = do
    lTy <- getType l
    rTy <- getType r
    case lTy of
        (TyFun lTy2 rTy2) -> case lTy2 == rTy of
            True -> pure rTy2
            False -> error $ printf "Type mismatch. Function expects %s but got %s." (ppType lTy2) (ppType rTy)
        _ -> error "Can't apply non-functions."
getType (Constr tag ty) = pure ty
getType (PrimFun pf) = pure $ getTypePF pf

getTypePF :: PrimFun -> Type
getTypePF (PrimBinOp op) = TyInt ~> TyInt ~> TyInt

insertIdent :: Ident -> M.Map String Type -> M.Map String Type
insertIdent (Id name ty) map = M.insert name ty map

buildTypeMap :: [Decl] -> M.Map String Type
buildTypeMap decls = M.fromList $ catMaybes $ map getType decls
    where
        getType (DTerm (Id name ty) _) = Just (name, ty)
        getType _ = Nothing