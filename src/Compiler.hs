{-# LANGUAGE PackageImports #-}
{- Compile a core program to Gcode. -}
module Compiler where

import Bound
import Bound.Name hiding (name)
import qualified Bound.Name (name)
import Bound.Scope
import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State

import AST
import GMachine

compile :: Module -> [(String, Instruction String, Int)]
compile mod = undefined

compileTopLevel :: (Ident, Expr Ident) -> (String, [Instruction String], Int)
compileTopLevel (ident, expr) = (name ident, sc expr, getArity expr)

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
r body env = c body env ++ [Slide $ length env + 1, Unwind]

c :: Expr Ident -> VarMap -> [Instruction String]
c (V ident) env = case lookup (name ident) env of
    Just n -> [Push n]
    Nothing -> [PushGlobal $ name ident]
c (L n) _ = [PushInt n]
c (l :@ r) env = c r env ++ c l (argOffset 1 env) ++ [MkAp]

argOffset :: Int -> VarMap -> VarMap
argOffset n env = [(i, n+m) | (i, m) <- env]
