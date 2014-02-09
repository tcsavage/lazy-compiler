{-# LANGUAGE PackageImports #-}

module Core.TypeCheck where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.State
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Text.Printf

import Core.AST
import Core.PrettyPrinter

-- | Check types. Simple enough.
typecheck :: Module -> Module
typecheck m = let emptyState = TypeCheck M.empty M.empty []
                  (m', state) = runState (initMod m >> processModule m) emptyState
              in case errors state of
                [] -> m'
                es -> error $ unlines $ reverse es

data TypeCheck = TypeCheck { typeMap :: M.Map String Type, kindMap :: M.Map String Kind, errors :: [String] }

type TypeCheckM a = State TypeCheck a

-- | Run an action inside its own inner scope. All types mapped inside this scope will not be accessible outside.
subScope :: TypeCheckM a -> TypeCheckM a
subScope action = do
    oldTypes <- gets typeMap
    oldKinds <- gets kindMap
    r <- action
    modify (\state -> state { typeMap = oldTypes, kindMap = oldKinds })
    pure r

-- | Add a type to the typeMap.
addType :: String -> Type -> TypeCheckM ()
addType name ty = do
    oldMap <- gets typeMap
    modify (\state -> state { typeMap = M.insert name ty oldMap })

-- | Add a type to the kindMap.
addKind :: String -> Kind -> TypeCheckM ()
addKind name ty = do
    oldMap <- gets kindMap
    modify (\state -> state { kindMap = M.insert name ty oldMap })

-- | Add an id to the type or kind map.
addIdent :: Ident -> TypeCheckM ()
addIdent (Id name ty) = addType name ty
addIdent (TyId name ki) = addKind name ki

lookupType :: String -> TypeCheckM (Maybe Type)
lookupType name = M.lookup name <$> gets typeMap

-- | Add an error to the log.
reportError :: String -> TypeCheckM ()
reportError e = do
    es <- gets errors
    modify (\state -> state { errors = e:es })

-- | Extract types of top-level term identifiers and kinds of top-level types, and add them to the state..
initMod :: Module -> TypeCheckM ()
initMod (Module _ decls) = do
    addType "dumpInt" (TyInt ~> TyInt)
    forM_ decls $ \decl -> case decl of
        DTerm ident _ -> addIdent ident
        DType ident constrs -> do
            addIdent ident
            mapM_ addIdent constrs

processModule :: Module -> TypeCheckM Module
processModule (Module name decls) = Module name <$> mapM processDecl decls

processDecl :: Decl -> TypeCheckM Decl
processDecl decl@(DTerm ident expr) = do
    ty <- subScope $ calculateType expr
    case ty == typeOf ident of
        True -> pure decl
        False -> do
            reportError $ printf "Type mismatch in top-level decl. Expected %s, got %s." (ppType $ typeOf ident) (ppType ty)
            pure decl
processDecl (DType ident constructors) = pure $ (DType ident constructors)  -- TODO: should probably check kinds here.

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
    -- Lookup name in typeMap.
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
            Type ty2 -> do
                ki <- calculateKind ty2
                case kindOf ident == ki of
                    True -> pure $ applyType (TyForAll ident ty1) ty2
                    False -> do
                        reportError $ printf "kind mismatch. Type application expecting type of kind %s but got kind %s." (ppKind $ kindOf ident) (ppKind ki)
                        pure ty2
            expr -> error $ printf "Syntax error. Expected type for application, got other expression instead: %s" (ppExpr expr)
calculateType (Lam ident@(Id name ty) expr) = subScope $ do
    addIdent ident
    ty2 <- calculateType expr
    pure $ ty ~> ty2
calculateType (Lam ident expr) = subScope $ do
    addIdent ident
    ty2 <- calculateType expr
    pure $ TyForAll ident ty2
calculateType (Let rec bindings expr) = subScope $ do
    forM_ bindings $ \(ident, bexpr) -> do
        addType (name ident) =<< checkIdent ident bexpr
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
calculateKind :: Type -> TypeCheckM Kind
calculateKind (TyFun l r) = pure KiStar
calculateKind (TyInt) = pure KiStar
calculateKind (TyVar ident) = pure $ kindOf ident
calculateKind (TyForAll ident ty) = calculateKind ty
calculateKind (TyAp tyL tyR) = do
    kiL <- calculateKind tyL
    case kiL of
        KiFun l r -> do
            kiR <- calculateKind tyR
            case kiR == r of
                True -> pure l
                False -> do
                    reportError $ printf "Kind mismatch. Applying type of kind %s where %s is expected." (ppKind kiR) (ppKind r)
                    pure r
        kind -> do
            reportError $ printf "Attempting to apply type of kind %s to concrete type %s." (ppKind kind) (ppType tyL)
            pure kind
