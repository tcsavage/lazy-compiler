{-# LANGUAGE PackageImports #-}

module TypeCheck where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Text.Printf

import AST
import PrettyPrinter

-- | Check types. Simple enough.
typecheck :: Module -> Module
typecheck m = let (m', state) = runState (processModule m) (TypeCheck (M.fromList $ getTopLevelTermTypes m) [])
              in case errors state of
                [] -> m'
                es -> error $ unlines $ reverse es

data TypeCheck = TypeCheck { namespace :: M.Map String Type, errors :: [String] }

type TypeCheckM a = State TypeCheck a

-- | Run an action inside its own inner scope. All types mapped inside this scope will not be accessible outside.
subScope :: TypeCheckM a -> TypeCheckM a
subScope action = do
    oldMap <- gets namespace
    r <- action
    modify (\state -> state { namespace = oldMap })
    pure r

-- | Add a type to the namespace map.
addName :: String -> Type -> TypeCheckM ()
addName name ty = do
    oldMap <- gets namespace
    modify (\state -> state { namespace = M.insert name ty oldMap })

addIdent :: Ident -> TypeCheckM ()
addIdent (Id name ty) = addName name ty
addIdent _ = error "Can only track types, not kinds."

lookupType :: String -> TypeCheckM (Maybe Type)
lookupType name = M.lookup name <$> gets namespace

-- | Add an error to the log.
reportError :: String -> TypeCheckM ()
reportError e = do
    es <- gets errors
    modify (\state -> state { errors = e:es })

-- | Extract types of top-level term identifiers.
getTopLevelTermTypes :: Module -> [(String, Type)]
getTopLevelTermTypes (Module _ decls) = [("dumpInt", TyInt ~> TyInt)] ++ (catMaybes $ map getTermDeclType decls)
    where
        getTermDeclType (DType _ _) = Nothing
        getTermDeclType (DTerm (Id name ty) _) = Just (name, ty)

processModule :: Module -> TypeCheckM Module
processModule (Module name decls) = Module name <$> mapM processDecl decls

processDecl :: Decl -> TypeCheckM Decl
processDecl decl@(DTerm ident expr) = do
    ty <- calculateType expr
    case ty == typeOf ident of
        True -> pure decl
        False -> do
            reportError $ printf "Type mismatch in top-level decl. Expected %s, got %s." (ppType $ typeOf ident) (ppType ty)
            pure decl
processDecl (DType ident constructors) = undefined

checkIdent :: Ident -> Expr Ident -> TypeCheckM Type
checkIdent (Id name ty1) expr = do
    ty2 <- calculateType expr
    case ty1 == ty2 of
        True -> pure ty1
        False -> do
            reportError $ printf "Type mismatch. Identifier has type %s but the expression has type %s." (ppType ty1) (ppType ty2)
            pure ty1

-- | Calculate the type of the given expression.
calculateType :: Expr Ident -> TypeCheckM Type
calculateType (V ident) = do
    -- Lookup name in namespace.
    mty <- lookupType $ name ident
    -- Make sure it's the same as the ident's.
    case mty of
        Just ty -> case ty == typeOf ident of
            True -> pure ty
            False -> do
                reportError $ printf "Type mismatch. Identifier has type %s but the value in scope has type %s." (ppType $ typeOf ident) (ppType ty)
                pure ty
        Nothing -> do
            reportError $ printf "Type lookup failed: %s." $ ppIdent ident
            pure $ typeOf ident
calculateType (L lit) = pure TyInt
calculateType (l :@ r) = do
    lTy <- calculateType l
    case lTy of
        -- Term application.
        TyFun lTy2 rTy2 -> do
            rTy <- calculateType r
            case lTy2 == rTy of
                True -> pure rTy2
                False -> do
                    reportError $ printf "Type mismatch. Function expecting value of type %s but got type %s." (ppType lTy2) (ppType rTy)
                    pure $ rTy2
        -- Type application.
        TyForAll ident ty1 -> case r of
            Type ty2 -> case kindOf ident == calculateKind ty2 of
                True -> pure $ applyType (TyForAll ident ty1) ty2
                False -> do
                    reportError $ printf "kind mismatch. Type application expecting type of kind %s but got kind %s." (ppType $ kindOf ident) (ppType $ calculateKind ty2)
                    pure ty2
            expr -> error $ printf "Syntax error. Expected type for application, got other expression instead: %s" (ppExpr expr)
calculateType (Lam ident@(Id name ty) expr) = subScope $ do
    addName name ty
    ty2 <- calculateType expr
    pure $ ty ~> ty2
calculateType (Lam ident@(TyId name ty) expr) = subScope $ do
    addName name ty
    ty2 <- calculateType expr
    pure $ TyForAll ident ty2
calculateType (Let rec bindings expr) = subScope $ do
    forM_ bindings $ \(ident, bexpr) -> do
        addName (name ident) =<< checkIdent ident bexpr
    calculateType expr
calculateType (Case expr ty alts) = do
    forM_ alts $ \(tag, binders, body) -> subScope $ do
        mapM_ addIdent binders
        ty2 <- calculateType body
        unless (ty == ty2) $ reportError $ printf "Type mismatch. Case alt has type %s where %s is expected." (ppType ty2) (ppType ty)
    pure ty
calculateType (Constr tag ty exprs)
    | length exprs == getTypeArity ty = do
        passed <- and <$> checkTypes ty exprs
        if passed then pure $ returnType ty else error "Type mismatch in data constructor."
    | length exprs > getTypeArity ty = error "Data constructor over-saturated."
    | length exprs < getTypeArity ty = error "Unsaturated data constructor."
  where
    checkTypes :: Type -> [Expr Ident] -> TypeCheckM [Bool]
    checkTypes (TyFun l r) (v:vs) = do
        ety <- calculateType v
        rest <- checkTypes r vs
        pure $ (l == ety) : rest
    checkTypes ty' (v:[]) = do
        ety <- calculateType v
        pure $ (ty' == ety) : []
    checkTypes ty' [] = pure []  -- Nullary constructors.
    checkTypes ty' vs = error $ printf "Foo: %s -- %s" (show ty') (show vs)
calculateType (PrimFun pf) = pure $ getTypePF pf
calculateType (Type ty) = error $ printf "Found type (%s) where term was expected." $ ppType ty

getTypePF :: PrimFun -> Type
getTypePF (PrimBinOp op) = TyInt ~> TyInt ~> TyInt

-- | Calculate the kind of a type.
calculateKind :: Type -> Kind
calculateKind = const TyKindStar  -- Cirrently only kind * is supported.
