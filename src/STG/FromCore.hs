{-# LANGUAGE PackageImports #-}

module STG.FromCore (core2stg) where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.State
import qualified Data.DList as D
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S

import qualified AST as Core
import AST (Expr((:@)))
import qualified STG.AST as STG
import Util

type Unique = Int

data Translator = Translator { newLets :: D.DList STG.Binding, nextUnique :: Unique, bound :: S.Set String }

type TranslatorM a = State Translator a

appendLets :: [STG.Binding] -> TranslatorM ()
appendLets bindings = do
    lets <- gets newLets
    modify (\translator -> translator { newLets = lets <> (D.fromList bindings) })

-- | Generate a new unique name.
genUnique :: TranslatorM String
genUnique = do
    un <- gets nextUnique
    modify (\translator -> translator { nextUnique = un+1 })
    pure ("u" ++ show un)

markBound :: String -> TranslatorM ()
markBound name = do
    bo <- gets bound
    modify (\translator -> translator { bound = S.insert name bo })

-- | Perform a TranslatorM action and restore old state of bound variables afterwards.
restoreBound :: TranslatorM a -> TranslatorM a
restoreBound comp = do
    state <- gets bound
    r <- comp
    modify (\translator -> translator { bound = state })
    pure r

tempBound :: [String] -> TranslatorM a -> TranslatorM a
tempBound names comp = restoreBound $ mapM_ markBound names >> comp

isFree :: String -> TranslatorM Bool
isFree name = S.notMember name <$> gets bound

-- | Generate a list of free variables in the given core expression.
getFree :: Core.Expr Core.Ident -> TranslatorM [String]
getFree (Core.V ident) = condM (isFree $ Core.name ident) (pure [Core.name ident]) (pure [])  -- If the variable isn't bound, it's free.
getFree (Core.L _) = pure []
getFree (l :@ r) = do
    lf <- getFree l
    rf <- getFree r
    pure $ nub (lf ++ rf)
getFree (Core.Lam binder expr) = restoreBound $ do
    markBound $ Core.name binder  -- Mark arg as bound.
    getFree expr
getFree (Core.Let _ bindings expr) = do
    bfrees <- concat <$> mapM getLetFree bindings
    efrees <- getFree expr
    pure $ nub (bfrees ++ efrees)
  where
    getLetFree :: (Core.Ident, Expr Core.Ident) -> TranslatorM [String]
    getLetFree (ident, expr) = tempBound [Core.name ident] $ getFree expr
getFree (Core.Case expr _ alts) = do
    efrees <- getFree expr
    afrees <- concat <$> mapM getAltFree alts
    pure $ nub (efrees ++ afrees)
  where
    getAltFree :: Core.Alt Core.Ident -> TranslatorM [String]
    getAltFree (tag, binders, expr) = tempBound (map Core.name binders) $ getFree expr
getFree (Core.Constr _ _ elems) = (nub . concat) <$> mapM getFree elems
getFree (Core.PrimFun _) = pure []

getFreeSTG :: STG.Expr -> TranslatorM [String]
getFreeSTG (STG.Let bindings expr) = restoreBound $ do
    bfrees <- forM bindings $ \(STG.Binding name lambda) -> do
        markBound name
        getFreeSTGLambda lambda
    efrees <- getFreeSTG expr
    pure $ nub (concat bfrees ++ efrees)
getFreeSTG (STG.LetRec bindings expr) = restoreBound $ do
    bfrees <- forM bindings $ \(STG.Binding name lambda) -> do
        markBound name
        getFreeSTGLambda lambda
    efrees <- getFreeSTG expr
    pure $ nub (concat bfrees ++ efrees)
getFreeSTG (STG.Case expr alts) = restoreBound $ do
    efrees <- getFreeSTG expr
    afrees <- case alts of
        STG.Algebraic aalts def -> do
            frees <- forM aalts $ \(STG.AAlt tag binders body) -> restoreBound $ do
                mapM_ markBound binders
                getFreeSTG body
            pure $ nub $ concat frees
        STG.Primitive palts def -> do
            frees <- forM palts $ \(STG.PAlt lit body) -> do
                getFreeSTG body
            pure $ nub $ concat frees
    pure $ nub (efrees ++ afrees)

getFreeSTG (STG.Ap name atoms) = do
    afrees <- concat <$> mapM getFreeSTGAtom atoms
    condM (isFree name) (pure (name:afrees)) (pure afrees)
getFreeSTG (STG.Constr tag elements) = concat <$> mapM getFreeSTGAtom elements
getFreeSTG (STG.Prim prim args) = concat <$> mapM getFreeSTGAtom args
getFreeSTG (STG.Literal _) = pure []

getFreeSTGLambda :: STG.Lambda -> TranslatorM [String]
getFreeSTGLambda (STG.Lambda frees update args expr) = restoreBound $ do
    mapM_ markBound args
    getFreeSTG expr

getFreeSTGAtom :: STG.Atom -> TranslatorM [String]
getFreeSTGAtom (STG.AtomLit _) = pure []
getFreeSTGAtom (STG.AtomVar name) = condM (isFree name) (pure [name]) (pure [])

floatLambda :: STG.Lambda -> TranslatorM String
floatLambda lam = do
    name <- genUnique
    appendLets [STG.Binding name lam]
    pure name

floatExpr :: STG.Expr -> TranslatorM String
floatExpr expr = do
    frees <- getFreeSTG expr
    floatLambda $ STG.Lambda frees STG.N [] expr

floatAtom :: STG.Expr -> TranslatorM STG.Atom
floatAtom expr = do
    name <- floatExpr expr
    pure $ STG.AtomVar name

-- | Evaluate the translator state for an STG lambda, inserting floated lets above the main body.
runTranslatorM :: [String] -> TranslatorM STG.Lambda -> STG.Lambda
runTranslatorM globals comp = flip evalState (Translator D.empty 0 (S.fromList globals)) $ do
    (STG.Lambda frees update args expr) <- comp
    bindings <- D.toList <$> gets newLets
    case bindings of
        [] -> pure $ STG.Lambda frees update args expr
        _ -> pure $ STG.Lambda frees update args $ STG.Let bindings expr

getBindingVar :: STG.Binding -> String
getBindingVar (STG.Binding name _) = name

core2stg :: Core.Module -> STG.Program
core2stg (Core.Module name decls) = STG.Program $ catMaybes $ map (translateDecl globals) decls
    where
        getName :: Core.Decl -> Maybe String
        getName (Core.DType _ _) = Nothing
        getName (Core.DTerm ident _) = Just $ Core.name ident
        globals = (catMaybes $ map getName decls) ++ ["dumpInt"]

translateDecl :: [String] -> Core.Decl -> Maybe STG.Binding
translateDecl _ (Core.DType _ _) = Nothing
translateDecl globals (Core.DTerm ident expr) = Just $ STG.Binding (Core.name ident) $ runTranslatorM globals $ translateLambda $ stripTypeAbstractions expr

translateLambda :: Core.Expr Core.Ident -> TranslatorM STG.Lambda
translateLambda (Core.Lam binder expr) = do
    -- Collect arg list and function body.
    let args = Core.name binder : buildArgList expr
    -- Mark each arg as bound in the following context.
    mapM_ markBound args
    -- Get free variables in the function body.
    texpr <- translateExpr $ getBody expr
    -- Generate STG for the function body.
    frees <- getFree expr
    -- Build lambda form.
    pure $ STG.Lambda frees STG.N args texpr
  where
    buildArgList :: Core.Expr Core.Ident -> [String]
    buildArgList (Core.Lam b e) = (Core.name b) : buildArgList e
    buildArgList e = []
    getBody :: Core.Expr Core.Ident -> Core.Expr Core.Ident
    getBody (Core.Lam _ e) = getBody e
    getBody e = e
translateLambda expr = do
    texpr <- translateExpr expr
    frees <- getFree expr
    pure $ STG.Lambda frees STG.N [] $ texpr

translateApplication :: Core.Expr Core.Ident -> TranslatorM STG.Expr
translateApplication expr = do
    atoms <- mapM translateAtom args
    case fun of
        Core.V ident -> pure $ STG.Ap (Core.name ident) atoms
        Core.PrimFun pf -> pure $ STG.Prim (translatePrimFun pf) atoms
        nonvar -> do
            var <- floatExpr =<< translateExpr nonvar
            pure $ STG.Ap var atoms
  where
    buildArgList :: Core.Expr a -> [Core.Expr a] -> [Core.Expr a]
    buildArgList (l :@ arg) acc = buildArgList l (arg : acc)
    buildArgList expr acc = expr : acc
    (fun:args) = buildArgList expr []

translateAtom :: Core.Expr Core.Ident -> TranslatorM STG.Atom
translateAtom (Core.V ident) = pure $ STG.AtomVar $ Core.name ident
translateAtom (Core.L lit) = pure $ STG.AtomLit lit
translateAtom ap@(l :@ r) = floatAtom =<< translateApplication ap
translateAtom expr = floatAtom =<< translateExpr expr

translateExpr :: Core.Expr Core.Ident -> TranslatorM STG.Expr
translateExpr (Core.V ident) = pure $ STG.Ap (Core.name ident) []
translateExpr (Core.L lit) = pure $ STG.Literal lit
translateExpr ap@(l :@ r) = translateApplication ap
translateExpr lam@(Core.Lam binder expr) = do
    lamName <- floatLambda =<< translateLambda lam
    pure $ STG.Ap lamName []
translateExpr (Core.Let True binders expr) = do
    bindings <- mapM translateLetRecBinding binders
    texpr <- translateExpr expr
    pure $ STG.LetRec bindings texpr
translateExpr (Core.Let False binders expr) = do
    bindings <- mapM translateLetBinding binders
    texpr <- translateExpr expr
    pure $ STG.Let bindings texpr
translateExpr (Core.Case expr _ alts) = do
    texpr <- translateExpr expr
    talts <- translateCaseAlts alts
    pure $ STG.Case texpr talts
translateExpr (Core.Constr tag _ elems) = do
    atoms <- mapM translateAtom elems
    pure $ STG.Constr tag atoms

translateLetBinding :: (Core.Ident, Core.Expr Core.Ident) -> TranslatorM STG.Binding
translateLetBinding (ident, expr) = do
    lam <- translateLambda expr
    pure $ STG.Binding (Core.name ident) lam

translateLetRecBinding = translateLetBinding

translateCaseAlts :: [Core.Alt Core.Ident] -> TranslatorM STG.Alts
translateCaseAlts alts = do
    talts <- mapM translateCaseAAlt alts
    pure $ STG.Algebraic talts (STG.Default (STG.Literal 0))  -- TODO: better default.

translateCaseAAlt :: Core.Alt Core.Ident -> TranslatorM STG.AAlt
translateCaseAAlt (tag, binders, expr) = restoreBound $ do
    let vars = map Core.name binders
    mapM_ markBound vars
    texpr <- translateExpr expr
    pure $ STG.AAlt tag vars texpr

translatePrimFun :: Core.PrimFun -> STG.Prim
translatePrimFun (Core.PrimBinOp pbo) = case pbo of
    Core.PrimAdd -> STG.PrimAdd
    Core.PrimMul -> STG.PrimMul

-- | Remove all big lambdas and type applications.
stripTypeAbstractions :: Core.Expr Core.Ident -> Core.Expr Core.Ident
stripTypeAbstractions (Core.Lam (Core.TyId name ki) expr) = stripTypeAbstractions expr
stripTypeAbstractions (l :@ (Core.Type ty)) = stripTypeAbstractions l
stripTypeAbstractions expr@(Core.V ident) = expr
stripTypeAbstractions expr@(Core.L lit) = expr
stripTypeAbstractions (l :@ r) = stripTypeAbstractions l :@ stripTypeAbstractions r
stripTypeAbstractions (Core.Lam ident expr) = Core.Lam ident $ stripTypeAbstractions expr
stripTypeAbstractions (Core.Let rec bindings expr) = Core.Let rec (map strip bindings) $ stripTypeAbstractions expr
    where
        strip :: (Core.Ident, Core.Expr Core.Ident) -> (Core.Ident, Core.Expr Core.Ident)
        strip (ident, expr) = (ident, stripTypeAbstractions expr)
stripTypeAbstractions (Core.Case expr ty alts) = (Core.Case (stripTypeAbstractions expr) ty (map strip alts))
    where
        strip :: Core.Alt Core.Ident -> Core.Alt Core.Ident
        strip (tag, binders, expr) = (tag, binders, stripTypeAbstractions expr)
stripTypeAbstractions (Core.Constr tag ty exprs) = Core.Constr tag ty $ map stripTypeAbstractions exprs
stripTypeAbstractions expr@(Core.PrimFun prim) = expr
stripTypeAbstractions expr@(Core.Type ty) = expr
