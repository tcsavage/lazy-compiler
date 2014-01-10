{-# LANGUAGE PackageImports #-}
{- Compile a core program to Gcode. -}
module Compiler where
    
import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import Data.Maybe

import AST
import GMachine
import PrettyPrinter

-- | Generate GCode from a core module.
compile :: Module -> [(String, [Instruction String], Int)]
compile mod = (catMaybes $ map compileTopLevel (_tlDecls mod)) ++ primOps

compileTopLevel :: Decl -> Maybe (String, [Instruction String], Int)
compileTopLevel (DTerm ident expr) = Just (name ident, sc expr, getArity expr)
compileTopLevel (DType _ _) = Nothing  -- Types are not compiled.

primOps :: [(String, [Instruction String], Int)]
primOps = map build [PrimBinOp PrimAdd, PrimBinOp PrimMul]
    where
        build pf = (ppPrimFun pf, compilePrimOp pf, getArity $ PrimFun pf)

type VarMap = [(String, Int)]

-- | Use the current state in a read-only computation.
readOnlyT :: Monad m => ReaderT s m a -> StateT s m a
readOnlyT comp = (lift . runReaderT comp) =<< get

readOnly :: Reader s a -> State s a
readOnly comp = fmap (runReader comp) get

buildEnvironment_ :: Expr Ident -> Int -> VarMap
buildEnvironment_ (Lam b expr) i = (name b, i) : (buildEnvironment_ expr (i+1))
buildEnvironment_ _ _ = []

buildEnvironment :: Expr Ident -> VarMap
buildEnvironment expr = buildEnvironment_ expr 0

getBody :: Expr Ident -> Expr Ident
getBody (Lam b expr) = getBody expr
getBody x = x

sc :: Expr Ident -> [Instruction String]
sc expr = r body env
    where
        env = buildEnvironment expr
        body = getBody expr

r :: Expr Ident -> VarMap -> [Instruction String]
r body env = e body env ++ [Update d, Pop d, Unwind]
    where
        d = length env

e :: Expr Ident -> VarMap -> [Instruction String]
e (L n) env = [PushInt n]
e (Let False decls expr) env = compileLet decls env ++ e expr env' ++ [Slide n]  -- Non-rec.
    where
        n = length decls
        env' = compileArgs decls env
e (Let True decls expr) env = [Alloc n] ++ compileLetrec decls env' ++ e expr env' ++ [Slide n]  -- Rec.
    where
        n = length decls
        env' = compileArgs decls env
e (Case expr  _ alts) env = e expr env ++ [CaseJump (d alts env)]
e (Constr tag ty values) env = concat (zipWith (\e n -> c e (argOffset n env)) (reverse values) [0..]) ++ [Pack tag arity]
    where
        arity = getTypeArity ty
e x env = c x env ++ [Eval]

c :: Expr Ident -> VarMap -> [Instruction String]
c (V ident) env = case lookup (name ident) env of
    Just n -> [Push n]
    Nothing -> [PushGlobal $ name ident]
c (L n) _ = [PushInt n]
c (l :@ r) env = c r env ++ c l (argOffset 1 env) ++ [MkAp]
c (Let False decls expr) env = compileLet decls env ++ c expr env' ++ [Slide n]  -- Non-rec.
    where
        n = length decls
        env' = compileArgs decls env
c (Let True decls expr) env = [Alloc n] ++ compileLetrec decls env' ++ c expr env' ++ [Slide n]  -- Rec.
    where
        n = length decls
        env' = compileArgs decls env
c (Constr tag ty values) env = concat (zipWith (\e n -> c e (argOffset n env)) (reverse values) [0..]) ++ [Pack tag arity]
    where
        arity = getTypeArity ty
c (PrimFun pf) env = [PushGlobal (ppPrimFun pf)]

d :: [Alt Ident] -> VarMap -> [(Int, [Instruction String])]
d alts env = map (flip a env) alts

a :: Alt Ident -> VarMap -> (Int, [Instruction String])
a (tag, binders, expr) env = (tag, Split n : e expr bindersEnv ++ [Slide n])
    where
        n = length binders
        bindersEnv = zipWith (\b n -> (name b, n)) binders [0..] ++ argOffset n env

compileLet :: [(Ident, Expr Ident)] -> VarMap -> [Instruction String]
compileLet [] _ = []
compileLet ((ident, expr):defs) env = c expr env ++ compileLet defs (argOffset 1 env)

compileLetrec :: [(Ident, Expr Ident)] -> VarMap -> [Instruction String]
compileLetrec [] _ = []
compileLetrec ((ident, expr):defs) env = c expr env ++ [Update $ length defs]

compileArgs :: [(Ident, Expr Ident)] -> VarMap -> VarMap
compileArgs defs env = zip (map (name . fst) defs) [n-1, n-2 .. 0] ++ argOffset n env
    where
        n = length defs

argOffset :: Int -> VarMap -> VarMap
argOffset n env = [(i, n+m) | (i, m) <- env]

compilePrimOp :: PrimFun -> [Instruction String]
compilePrimOp (PrimBinOp op) = [Push 1, Eval, Push 1, Eval, compilePrimBinOp op, Update 2, Pop 2, Unwind]

compilePrimBinOp :: PrimBinOp -> Instruction String
compilePrimBinOp PrimAdd = Add
compilePrimBinOp PrimMul = Mul
